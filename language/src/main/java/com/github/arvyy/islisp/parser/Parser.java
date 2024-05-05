package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.functions.ISLISPDefaultHandler;
import com.github.arvyy.islisp.nodes.*;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

import java.io.IOException;
import java.math.BigInteger;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * Parser builds the nodes for the program's AST.
 */
public class Parser {

    private final Set<String> moduleLoadInProgress;
    private final Map<EqWrapper, SourceSection> sourceSectionMap;

    /**
     * Create parser.
     */
    public Parser() {
        sourceSectionMap = new HashMap<>();
        moduleLoadInProgress = new HashSet<>();
    }

    /**
     * Returns root node for given main module represented by given source.
     * Actual code content isn't parsed, but deferred by wrapping into
     * ISLISPModuleNode node since parsing requires executing macros,
     * which potentially require executing user code,
     * which is not supposed to be executed in parse.
     * Handles installing base condition handler.
     *
     * @param language language reference
     * @param module the 'main' module name that will entrypoint
     * @param source source for the module. If source is marked as interactive,
     *               in case of unexpected error do not exit with status code
     * @return root node
     */
    //TODO rename
    public ISLISPRootNode createMainModuleNode(ISLISPTruffleLanguage language, String module, Source source) {
        var topLevelConditionHandler = new ISLISPWithHandlerNode(
            new ISLISPLiteralNode(ISLISPDefaultHandler.makeLispFunction(language, source.isInteractive()), null),
            new ISLISPExpressionNode[]{new ISLISPModuleNode(this, parseModuleSource(module, source))},
            null
        );
        return new ISLISPRootNode(
                language,
                new ISLISPExpressionNode[]{topLevelConditionHandler},
                null
        );
    }

    /**
     * Returns root node for evaluating inline
     * during paused execution in debugger.
     *
     * @param language language reference
     * @param debuggerNode debugger node at which the execution is being paused
     * @param source custom source to eval at the context of the paused debugger node
     * @return executable node wrapping evaluation of local parse.
     */
    public ISLISPRootNode createInlineDebuggerEvalNode(
        ISLISPTruffleLanguage language,
        ISLISPDebuggerNode debuggerNode,
        Source source
    ) {
        var reader = new Reader(source, sourceSectionMap);
        var sexprs = reader.readAll();
        var exprs = new ArrayList<ISLISPExpressionNode>();
        for (var sexpr: sexprs) {
            var expression = parseExpressionNode(debuggerNode.getParserContext(), sexpr, false);
            exprs.add(expression);
        }
        return new ISLISPRootNode(language, exprs.toArray(ISLISPExpressionNode[]::new), null);
    }

    @CompilerDirectives.TruffleBoundary
    ModuleSource parseModuleSource(String name, Source source) {
        var reader = new Reader(source, sourceSectionMap);
        var content = reader.readAll();
        var rest = new ArrayList<>(content.size());
        var requires = new ArrayList<String>();
        var provides = new ArrayList<SymbolReference>();
        for (var obj: content) {
            if (obj instanceof Pair p && p.car() instanceof Symbol s) {
                if ("requires".equals(s.name())) {
                    var requireList = requireList(obj, -1, -1);
                    for (var req: requireList.subList(1, requireList.size())) {
                        requires.add(downcast(req, String.class));
                    }
                    continue;
                }
                if ("provides".equals(s.name())) {
                    var provideList = requireList(obj, -1, -1);
                    for (var provide: provideList.subList(1, provideList.size())) {
                        provides.add(downcast(provide, Symbol.class).identityReference());
                    }
                    continue;
                }
            }
            rest.add(obj);
        }
        return new ModuleSource(name, source.createSection(0, source.getLength()), requires, provides, rest);
    }

    SourceSection span(SourceSection a, SourceSection b) {
        if (a == null || b == null) {
            return null;
        }
        return a.getSource().createSection(
                a.getStartLine(), a.getStartColumn(),
                b.getEndLine(), b.getEndColumn()
        );
    }

    /**
     * Expand, parse, and execute given sexprs.
     * This is deferredly called from ISLISPModuleNode, because macro expansion
     * might require execution of user code.
     * The expanded result must be spliced into node tree using exprCallback
     * to enable instrumentation.
     *
     * @param module module to which given sexprs belong
     * @param sexprs source code as a list of sexprs
     * @param exprCallback callback to run upon evaluation of each top level sexpr
     * @return value of last expression
     */
    public Object expandAndExecute(String module, List<Object> sexprs, Consumer<ISLISPExpressionNode> exprCallback) {
        var parserContext = new ParserContext(module);
        Object last = ISLISPContext.get(null).getNil();
        for (var v: sexprs) {
            var expression = parseExpressionNode(parserContext, v, true);
            last = executeExpression(expression, parserContext.frameBuilder.build());
            exprCallback.accept(expression);
        }
        return last;
    }

    Object executeExpression(ISLISPExpressionNode expression, FrameDescriptor fd) {
        var root = new ISLISPRootNode(
            null,
            new ISLISPExpressionNode[]{expression},
            fd);
        return root.getCallTarget().call();
    }

    void executeDefinitions(ISLISPExpressionNode expression, FrameDescriptor fd) {
        if (expression.isDefinitionNode()) {
            var root = new ISLISPRootNode(
                    null,
                    new ISLISPExpressionNode[]{expression},
                    fd);
            root.getCallTarget().call();
        }
        if (expression instanceof ISLISPPrognNode) {
            for (var e: ((ISLISPPrognNode) expression).getBodyNodes()) {
                executeDefinitions(e, fd);
            }
        }
    }

    ISLISPExpressionNode parseExpressionNode(ParserContext parserContext, Object sexpr) {
        return parseExpressionNode(parserContext, sexpr, false);
    }

    ISLISPExpressionNode parseExpressionNode(ParserContext parserContext, Object sexpr, boolean topLevel) {
        if (sexpr instanceof Pair form && ((Pair) sexpr).car() instanceof Symbol symbol) {
            var carName = ((Symbol) ((Pair) sexpr).car()).name();
            if (!topLevel) {
                switch (carName) {
                    case "defclass":
                    case "defconstant":
                    case "defdynamic":
                    case "defgeneric":
                    case "defglobal":
                    case "defmacro":
                    case "defun":
                    case "defmethod":
                        throw new ParsingException(
                            source(sexpr),
                            String.format("%s required to be at top level.", carName));
                    default:
                }
            }
            // builtins
            switch (carName) {
                case "assure":
                case "the":
                    return parseAssureNode(parserContext, sexpr);
                case "defclass":
                    return parseDefClass(parserContext, sexpr);
                case "defconstant":
                    return parseDefConstant(parserContext, sexpr);
                case "defdynamic":
                    return parseDefDynamic(parserContext, sexpr);
                case "defgeneric":
                    return parseDefGeneric(parserContext, sexpr);
                case "defglobal":
                    return parseDefGlobal(parserContext, sexpr);
                case "defmacro":
                    return parseDefMacro(parserContext, sexpr);
                case "defun":
                    return parseDefun(parserContext, sexpr);
                case "defmethod":
                    return parseDefMethod(parserContext, sexpr);
                case "catch":
                    return parseCatchNode(parserContext, sexpr);
                case "throw":
                    return parseThrowNode(parserContext, sexpr);
                case "convert":
                    return parseConvert(parserContext, sexpr);
                case "progn":
                    return parseProgn(parserContext, sexpr, topLevel);
                case "funcall":
                    return parseIndirectFunCall(parserContext, sexpr, false);
                case "apply":
                    return parseIndirectFunCall(parserContext, sexpr, true);
                case "function":
                    return parseFunctionRef(parserContext, sexpr);
                case "lambda":
                    return parseLambda(parserContext, sexpr);
                case "if":
                    return parseIfNode(parserContext, sexpr);
                case "debugger":
                    return parseDebuggerNode(parserContext, sexpr);
                case "quote":
                    return parseQuote(parserContext, sexpr);
                case "quasiquote":
                    return parseQuasiquote(parserContext, sexpr);
                case "class":
                    return parseClassRef(parserContext, sexpr);
                case "cond":
                    return parseCond(parserContext, sexpr);
                case "case":
                    return parseCase(parserContext, sexpr);
                case "case-using":
                    return parseCaseUsing(parserContext, sexpr);
                case "flet":
                    return parseFletNode(parserContext, sexpr);
                case "for":
                    return parseFor(parserContext, sexpr);
                case "block":
                    return parseBlock(parserContext, sexpr);
                case "return-from":
                    return parseReturnFrom(parserContext, sexpr);
                case "labels":
                    return parseLabelsNode(parserContext, sexpr);
                case "let":
                    return parseLetNode(parserContext, sexpr);
                case "let*":
                    return parseLetStarNode(parserContext, sexpr);
                case "dynamic":
                    return parseDynamic(parserContext, sexpr);
                case "dynamic-let":
                    return parseDynamicLet(parserContext, sexpr);
                case "set-dynamic":
                    return parseSetDynamic(parserContext, sexpr);
                case "setq":
                    return parseSetq(parserContext, sexpr);
                case "tagbody":
                    return parseTagBody(parserContext, sexpr);
                case "go":
                    return parseTagBodyGo(parserContext, sexpr);
                case "unwind-protect":
                    return parseUnwindProtectNode(parserContext, sexpr);
                case "while":
                    return parseWhile(parserContext, sexpr);
                case "with-error-output":
                    return parseWithErrorOutput(parserContext, sexpr);
                case "with-handler":
                    return parseWithHandler(parserContext, sexpr);
                case "with-standard-input":
                    return parseWithStandardInput(parserContext, sexpr);
                case "with-standard-output":
                    return parseWithStandardOutput(parserContext, sexpr);

                default:
            }
            // macros
            if ("setf".equals(carName)) {
                var args = requireList(form, 3, 3);
                var place = macroExpand(parserContext.module, args.get(1), false);
                var value = args.get(2);
                if (place instanceof Symbol s) {
                    var setq = Utils.listToValue(List.of(
                        ISLISPContext.get(null).namedSymbol("setq"),
                        s,
                        value
                    ));
                    sourceSectionMap.put(new EqWrapper(setq), source(sexpr));
                    return parseExpressionNode(parserContext, setq);
                }
                var placeList = requireList(place, 1, -1);
                var setfDispatchSymbol = downcast(placeList.get(0), Symbol.class);
                var setfDispatch = ISLISPContext.get(null)
                        .lookupSetfTransformer(parserContext.module, setfDispatchSymbol.identityReference());
                if (setfDispatch == null) {
                    return parseDirectSetfFunctionCall(parserContext, sexpr, placeList, value);
                }
                var transformed = setfDispatch.transform(placeList, value);
                sourceSectionMap.put(new EqWrapper(transformed), source(sexpr));
                return parseExpressionNode(
                        parserContext,
                        transformed);
            }
            var expanded = macroExpand(parserContext.module, sexpr, true);
            if (sexpr == expanded) {
                return parseDirectFunctionCall(parserContext, sexpr);
            } else {
                return parseExpressionNode(parserContext, expanded, topLevel);
            }
        }
        //a list pattern, but car isn't a symbol: it might be immediate lambda call
        if (sexpr instanceof Pair) {
            var car = ((Pair) sexpr).car();
            if (car instanceof Pair firstPos) {
                if (firstPos.car() instanceof Symbol maybeLambda) {
                    if (maybeLambda.name().equals("lambda")) {
                        var lambda = parseLambda(parserContext, firstPos);
                        return parseDirectLambdaCall(
                                parserContext,
                                lambda,
                                sexpr);
                    }
                }
            }
        }
        if (sexpr instanceof Integer
            || sexpr instanceof Double
            || sexpr instanceof LispChar
            || sexpr instanceof String
            || sexpr instanceof LispVector
            || sexpr instanceof LispArray
        ) {
            return new ISLISPLiteralNode(sexpr, null);
        }
        if (sexpr instanceof BigInteger b) {
            return new ISLISPLiteralNode(new LispBigInteger(b), null);
        }
        if (sexpr instanceof Symbol symbol) {
            var maybeLexicalSlot = parserContext.variables.get(symbol.identityReference());
            if (maybeLexicalSlot.isPresent()) {
                var variableContext = maybeLexicalSlot.get();
                var index = parserContext.frameDepth - variableContext.frameDepth;
                var slot = variableContext.slot;
                return new ISLISPLexicalIdentifierNode(index, slot, source(sexpr));
            } else {
                return new ISLISPGlobalIdentifierNode(parserContext.module, symbol, source(sexpr));
            }
        }
        throw new ParsingException(source(sexpr), "Unrecognized form.");
    }

    private ISLISPConvertNode parseConvert(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 3);
        var valueExpr = parseExpressionNode(parserContext, args.get(1));
        var className = downcast(args.get(2), Symbol.class);
        return new ISLISPConvertNode(
            parserContext.module,
            valueExpr,
            className,
            source(sexpr)
        );
    }

    private ISLISPCaseNode parseCaseUsing(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, -1);
        return parseCaseHelper(
            parserContext,
            parseExpressionNode(parserContext, args.get(1)),
            parseExpressionNode(parserContext, args.get(2)),
            args.subList(3, args.size()),
            source(sexpr)
        );
    }

    private ISLISPCaseNode parseCase(ParserContext parserContext, Object sexpr) {
        var ctx = ISLISPContext.get(null);
        var args = requireList(sexpr, 2, -1);
        return parseCaseHelper(
            parserContext,
            new ISLISPFunctionRefNode("ROOT", ctx.namedSymbol("eql"), null),
            parseExpressionNode(parserContext, args.get(1)),
            args.subList(2, args.size()),
            source(sexpr)
        );
    }

    private ISLISPCaseNode parseCaseHelper(
        ParserContext parserContext,
        ISLISPExpressionNode comparatorFn,
        ISLISPExpressionNode keyForm,
        List<Object> cases,
        SourceSection sourceSection
    ) {
        var ctx = ISLISPContext.get(null);
        ISLISPExpressionNode[] elseExprs = null;
        var keys = new ArrayList<Object[]>();
        var exprs = new ArrayList<ISLISPExpressionNode[]>();
        for (var c: cases) {
            var caseList = requireList(c, 1, -1);
            if (caseList.get(0) instanceof Symbol s
                && s.identityReference() == ctx.namedSymbol("t").identityReference()
            ) {
                elseExprs = caseList.stream()
                    .skip(1)
                    .map(sexpr -> parseExpressionNode(parserContext, sexpr))
                    .toArray(ISLISPExpressionNode[]::new);
            } else {
                if (elseExprs != null) {
                    throw new ParsingException(sourceSection, "Trailing cases after 'else' case");
                }
                var caseKeysList = requireList(caseList.get(0), -1, -1);
                keys.add(caseKeysList.toArray());
                var caseExprs = caseList.stream()
                    .skip(1)
                    .map(sexpr -> parseExpressionNode(parserContext, sexpr))
                    .toArray(ISLISPExpressionNode[]::new);
                exprs.add(caseExprs);
            }
        }
        return new ISLISPCaseNode(
            keyForm,
            comparatorFn,
            exprs.toArray(ISLISPExpressionNode[][]::new),
            keys.toArray(Object[][]::new),
            elseExprs == null ? new ISLISPExpressionNode[0] : elseExprs,
            sourceSection
        );
    }

    private ISLISPCondNode parseCond(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, -1, -1);
        var content = args.stream()
            .skip(1)
            .map(clause -> {
                return requireList(clause, 1, -1)
                    .stream()
                    .map(e -> parseExpressionNode(parserContext, e))
                    .toArray(ISLISPExpressionNode[]::new);
            })
            .toArray(ISLISPExpressionNode[][]::new);
        return new ISLISPCondNode(content, source(sexpr));
    }

    private ISLISPAssureNode parseAssureNode(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 3);
        var className = downcast(args.get(1), Symbol.class);
        var expression = parseExpressionNode(parserContext, args.get(2));
        return new ISLISPAssureNode(className, expression, source(sexpr));
    }

    private ISLISPExpressionNode parseWithStandardOutput(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        var outputstreamExpr = parseExpressionNode(parserContext, args.get(1));
        var bodyExprs = args.stream()
            .skip(2)
            .map(s -> parseExpressionNode(parserContext, s))
            .toArray(ISLISPExpressionNode[]::new);
        return new ISLISPWithStandardOutputNode(outputstreamExpr, bodyExprs, source(sexpr));
    }

    private ISLISPExpressionNode parseWithStandardInput(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        var inputstreamExpr = parseExpressionNode(parserContext, args.get(1));
        var bodyExprs = args.stream()
            .skip(2)
            .map(s -> parseExpressionNode(parserContext, s))
            .toArray(ISLISPExpressionNode[]::new);
        return new ISLISPWithStandardInputNode(inputstreamExpr, bodyExprs, source(sexpr));
    }

    private ISLISPExpressionNode parseWithErrorOutput(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        var outputstreamExpr = parseExpressionNode(parserContext, args.get(1));
        var bodyExprs = args.stream()
            .skip(2)
            .map(s -> parseExpressionNode(parserContext, s))
            .toArray(ISLISPExpressionNode[]::new);
        return new ISLISPWithErrorOutputNode(outputstreamExpr, bodyExprs, source(sexpr));
    }

    private ISLISPForNode parseFor(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, -1);
        var slots = new ArrayList<Integer>();
        var inits = new ArrayList<ISLISPExpressionNode>();
        var steps = new ArrayList<ISLISPExpressionNode>();
        var iterationSpecList = requireList(args.get(1), -1, -1);
        var newLexicalScope = new HashMap<SymbolReference, ParserContext.VariableContext>();
        for (var variableSpec: iterationSpecList) {
            var varSpecList = requireList(variableSpec, 2, 3);
            var varName = downcast(varSpecList.get(0), Symbol.class);
            inits.add(parseExpressionNode(parserContext, varSpecList.get(1)));
            var slot = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
            slots.add(slot);
            var variableContext = new ParserContext.VariableContext();
            variableContext.slot = slot;
            variableContext.frameDepth = parserContext.frameDepth;
            variableContext.name = varName.name();
            newLexicalScope.put(varName.identityReference(), variableContext);
        }
        var internalParserContext = parserContext.pushLexicalScope(newLexicalScope);
        for (var variableSpec : iterationSpecList) {
            var varSpecList = requireList(variableSpec, 2, 3);
            var varName = downcast(varSpecList.get(0), Symbol.class);
            var step = varSpecList.size() == 2 ? varName : varSpecList.get(2);
            var stepExpression = parseExpressionNode(internalParserContext, step);
            stepExpression.setParserContext(internalParserContext);
            steps.add(stepExpression);
        }
        var endTestGroup = requireList(args.get(2), 1, -1);
        var testExpr = parseExpressionNode(internalParserContext, endTestGroup.get(0));
        testExpr.setParserContext(internalParserContext);
        var resultBody = endTestGroup.stream()
            .skip(1)
            .map(resultExpr -> parseExpressionNode(internalParserContext, resultExpr))
            .peek(e -> e.setParserContext(internalParserContext));
        var iterationBody = args.stream()
            .skip(3)
            .map(e -> parseExpressionNode(internalParserContext, e))
            .peek(e -> e.setParserContext(internalParserContext));
        return new ISLISPForNode(
            slots.stream().mapToInt(i -> i).toArray(),
            inits.toArray(ISLISPExpressionNode[]::new),
            steps.toArray(ISLISPExpressionNode[]::new),
            iterationBody.toArray(ISLISPExpressionNode[]::new),
            testExpr,
            resultBody.toArray(ISLISPExpressionNode[]::new),
            source(sexpr)
        );
    }

    private ISLISPWhileNode parseWhile(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        var testExpression = parseExpressionNode(parserContext, args.get(1));
        var body = args.stream()
            .skip(2)
            .map(e -> parseExpressionNode(parserContext, e))
            .toArray(ISLISPExpressionNode[]::new);
        return new ISLISPWhileNode(testExpression, body, source(sexpr));
    }

    private ISLISPWithHandlerNode parseWithHandler(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        var handlerExpression = parseExpressionNode(parserContext, args.get(1));
        var body = args.stream()
            .skip(2)
            .map(e -> parseExpressionNode(parserContext, e))
            .toArray(ISLISPExpressionNode[]::new);
        return new ISLISPWithHandlerNode(handlerExpression, body, source(sexpr));
    }

    private ISLISPThrowNode parseThrowNode(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 3);
        var tagForm = parseExpressionNode(parserContext, args.get(1));
        var resultForm = parseExpressionNode(parserContext, args.get(2));
        return new ISLISPThrowNode(tagForm, resultForm, source(sexpr));
    }

    ISLISPCatchNode parseCatchNode(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        var tagForm = parseExpressionNode(parserContext, args.get(1));
        var forms = args.stream()
                .skip(2)
                .map(v -> parseExpressionNode(parserContext, v))
                .toArray(ISLISPExpressionNode[]::new);
        return new ISLISPCatchNode(tagForm, forms, source(sexpr));
    }

    ISLISPDefGlobalNode parseDefGlobal(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 3);
        var name = downcast(args.get(1), Symbol.class);
        var init = parseExpressionNode(parserContext, args.get(2));
        return new ISLISPDefGlobalNode(parserContext.module, name, init, source(sexpr));
    }

    ISLISPDefConstantNode parseDefConstant(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 3);
        var name = downcast(args.get(1), Symbol.class);
        var init = parseExpressionNode(parserContext, args.get(2));
        return new ISLISPDefConstantNode(parserContext.module, name, init, source(sexpr));
    }

    ISLISPTagBodyGoNode parseTagBodyGo(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, 2);
        var tagSymbol = downcast(args.get(1), Symbol.class);
        var maybeTagId = parserContext.tagbodyTags.get(tagSymbol.identityReference());
        if (maybeTagId.isPresent()) {
            return new ISLISPTagBodyGoNode(maybeTagId.get(), source(sexpr));
        }
        throw new ParsingException(source(sexpr), "Not found tag " + tagSymbol);
    }

    ISLISPTagBodyNode parseTagBody(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 1, -1);
        var expressions = new ArrayList<Object>();
        var tags = new HashMap<SymbolReference, Integer>();
        var tagSymbols = new ArrayList<SymbolReference>();
        for (var arg: args.subList(1, args.size())) {
            if (arg instanceof Symbol tag) {
                tags.put(tag.identityReference(), expressions.size());
                tagSymbols.add(tag.identityReference());
            } else if (arg instanceof Pair) {
                expressions.add(arg);
            } else {
                throw new ParsingException(source(arg), "Tagbody part neither a symbol nor a complex expression.");
            }
        }
        var newContext = parserContext.pushTagbodyScope(tagSymbols);
        var tagIds = new int[tagSymbols.size()];
        var tagPosition = new int[tagSymbols.size()];
        for (int i = 0; i < tagSymbols.size(); i++) {
            var tagSymbol = tagSymbols.get(i);
            tagIds[i] = newContext.tagbodyTags.get(tagSymbol).get();
            tagPosition[i] = tags.get(tagSymbol);
        }
        var parsedExpressions = new ISLISPExpressionNode[expressions.size()];
        for (int i = 0; i < expressions.size(); i++) {
            parsedExpressions[i] = parseExpressionNode(newContext, expressions.get(i));
        }
        return new ISLISPTagBodyNode(tagIds, tagPosition, parsedExpressions, source(sexpr));
    }

    ISLISPSetqNode parseSetq(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 3);
        var name = downcast(args.get(1), Symbol.class);
        var expr = parseExpressionNode(parserContext, args.get(2));
        var maybeVar = parserContext.variables.get(name.identityReference());
        if (maybeVar.isPresent()) {
            var variableContext = maybeVar.get();
            var index = parserContext.frameDepth - variableContext.frameDepth;
            return new ISLISPSetqNode(parserContext.module, index, variableContext.slot, expr, source(sexpr));
        } else {
            return new ISLISPSetqNode(parserContext.module, name, expr, source(sexpr));
        }
    }

    Object macroExpand(String module, Object form, boolean single) {
        if (form instanceof Pair p && p.car() instanceof Symbol symbol) {
            var rest = p.cdr();
            var maybeMacro = ISLISPContext.get(null).lookupMacro(module, symbol.identityReference());
            if (maybeMacro != null) {
                var args = new ArrayList<Object>();
                args.add(null); // closure param
                Iterable<Object> it = rest.equals(ISLISPContext.get(null).getNil()) ? List.of() : (Pair) rest;
                for (var e: it) {
                    args.add(e);
                }
                var transformedSexpr = maybeMacro.callTarget().call(args.toArray());
                if (transformedSexpr instanceof Pair tp && !single) {
                    var parts = Utils.readList(tp);
                    var newParts = parts.stream()
                            .map(part -> macroExpand(module, part, false))
                            .collect(Collectors.toList());
                    var transformedValue = Utils.listToValue(newParts);
                    sourceSectionMap.put(new EqWrapper(transformedValue), source(form));
                    return transformedValue;
                } else {
                    if (transformedSexpr instanceof Pair || transformedSexpr instanceof Symbol) {
                        sourceSectionMap.put(new EqWrapper(transformedSexpr), source(form));
                    }
                    return transformedSexpr;
                }
            }
        }
        return form;
    }

    ISLISPSetDynamicNode parseSetDynamic(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 3);
        var initalizer = parseExpressionNode(parserContext, args.get(1));
        var symbol = (Symbol) args.get(2);
        return new ISLISPSetDynamicNode(parserContext.module, symbol, initalizer, source(sexpr));
    }

    ISLISPDynamicLetNode parseDynamicLet(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        var bindingList = requireList(args.get(1), -1, -1);
        var symbols = new Symbol[bindingList.size()];
        var initializers = new ISLISPExpressionNode[bindingList.size()];
        for (int i = 0; i < symbols.length; i++) {
            var bindingEntry = requireList(bindingList.get(i), 2, 2);
            symbols[i] = downcast(bindingEntry.get(0), Symbol.class);
            initializers[i] = parseExpressionNode(parserContext, bindingEntry.get(1));
        }
        var body = new ISLISPExpressionNode[args.size() - 2];
        for (int i = 0; i < body.length; i++) {
            body[i] = parseExpressionNode(parserContext, args.get(i + 2));
        }
        return new ISLISPDynamicLetNode(parserContext.module, symbols, initializers, body, source(sexpr));
    }

    ISLISPDynamicLookupNode parseDynamic(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, 2);
        var name = downcast(args.get(1), Symbol.class);
        return new ISLISPDynamicLookupNode(parserContext.module, name, source(sexpr));
    }

    ISLISPDefDynamicNode parseDefDynamic(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 3);
        return new ISLISPDefDynamicNode(
                parserContext.module,
                downcast(args.get(1), Symbol.class),
                parseExpressionNode(parserContext, args.get(2)), source(sexpr));
    }

    ISLISPDefMethodNode parseDefMethod(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, -1);
        Symbol name;
        boolean setf;
        if (args.get(1) instanceof Symbol s) {
            name = s;
            setf = false;
        } else {
            var setfForm = requireList(args.get(1), 2, 2);
            if (!(setfForm.get(0) instanceof Symbol setfSymbol) || !setfSymbol.name().equals("setf")) {
                throw new ParsingException(source(sexpr), "Bad defmethod function spec");
            }
            name = downcast(setfForm.get(1), Symbol.class);
            setf = true;
        }
        var methodQualifiers = new ArrayList<String>();
        for (int i = 2; i < args.size(); i++) {
            if (args.get(i) instanceof Symbol s && !s.name().equals("nil")) {
                methodQualifiers.add(s.name());
            } else {
                break;
            }
        }
        var paramListIndex = 2 + methodQualifiers.size();
        var parameterList = requireList(args.get(paramListIndex), -1, -1);
        var plainParamList = new ArrayList<Symbol>();
        // collect defmethod argument list into normal lambda list to resolve slots
        for (var el: parameterList) {
            if (el instanceof Symbol s) {
                plainParamList.add(s);
            } else if (el instanceof Pair p) {
                var paramWithType = requireList(p, 2, 2);
                plainParamList.add(downcast(paramWithType.get(0), Symbol.class));
            }
        }
        parserContext = parserContext.pushFrameDescriptor();
        var slotsAndNewContext = processFrameDescriptorsForFunctionArguments(parserContext, plainParamList);
        // we know named slot count; collect their types to use for dispatch
        List<Symbol> paramTypes = new ArrayList<>();
        for (int i = 0; i < slotsAndNewContext.namedArgsSlots.length; i++) {
            var el = parameterList.get(i);
            if (el instanceof Symbol s) {
                paramTypes.add(ISLISPContext.get(null).namedSymbol("<object>"));
            } else if (el instanceof Pair p) {
                var paramWithType = requireList(p, 2, 2);
                paramTypes.add(downcast(paramWithType.get(1), Symbol.class));
            }
        }
        ParserContext bodyParserContext;
        int callNextMethodSlot;
        int hasNextMethodSlot;
        ISLISPDefMethodNode.MethodQualifier methodQualifier = ISLISPDefMethodNode.MethodQualifier.none;
        if (methodQualifiers.contains(":before")) {
            if (methodQualifier != ISLISPDefMethodNode.MethodQualifier.none) {
                throw new ParsingException(source(sexpr), "Incompatible method qualifiers");
            }
            methodQualifier = ISLISPDefMethodNode.MethodQualifier.before;
        }
        if (methodQualifiers.contains(":after")) {
            if (methodQualifier != ISLISPDefMethodNode.MethodQualifier.none) {
                throw new ParsingException(source(sexpr), "Incompatible method qualifiers");
            }
            methodQualifier = ISLISPDefMethodNode.MethodQualifier.after;
        }
        if (methodQualifiers.contains(":around")) {
            if (methodQualifier != ISLISPDefMethodNode.MethodQualifier.none) {
                throw new ParsingException(source(sexpr), "Incompatible method qualifiers");
            }
            methodQualifier = ISLISPDefMethodNode.MethodQualifier.around;
        }
        if (methodQualifier == ISLISPDefMethodNode.MethodQualifier.before
                || methodQualifier == ISLISPDefMethodNode.MethodQualifier.after
        ) {
            callNextMethodSlot = -1;
            hasNextMethodSlot = -1;
            bodyParserContext = parserContext;
        } else {
            var callNextMethodVar = new ParserContext.VariableContext();
            callNextMethodVar.slot = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
            callNextMethodVar.frameDepth = 0;
            callNextMethodVar.name = "call-next-method";
            var nextMethodPVar = new ParserContext.VariableContext();
            nextMethodPVar.slot = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
            nextMethodPVar.frameDepth = 0;
            nextMethodPVar.name = "next-method-p";
            bodyParserContext = slotsAndNewContext.context.pushLexicalFunctionScope(Map.of(
                    ISLISPContext.get(null).namedSymbol("next-method-p").identityReference(), nextMethodPVar,
                    ISLISPContext.get(null).namedSymbol("call-next-method").identityReference(), callNextMethodVar
            ));
            callNextMethodSlot = callNextMethodVar.slot;
            hasNextMethodSlot = nextMethodPVar.slot;
        }
        var bodyStatements = args.stream()
                .skip(paramListIndex + 1)
                .map(v -> parseExpressionNode(bodyParserContext, v))
                .toArray(ISLISPExpressionNode[]::new);
        var body = new ISLISPPrognNode(
                bodyStatements,
                span(
                        bodyStatements[0].getSourceSection(),
                        bodyStatements[bodyStatements.length - 1].getSourceSection()));
        body.setParserContext(bodyParserContext);
        var ctx = ISLISPContext.get(null);
        var userDefinedFunctionNode = new ISLISPUserDefinedFunctionNode(
                ctx.getLanguage(),
                body,
                slotsAndNewContext.namedArgsSlots,
                slotsAndNewContext.restArgsSlot,
                callNextMethodSlot,
                hasNextMethodSlot,
                source(sexpr));
        var rootNode = new ISLISPRootNode(
                ctx.getLanguage(),
                new ISLISPExpressionNode[]{userDefinedFunctionNode},
                parserContext.frameBuilder.build());
        return new ISLISPDefMethodNode(
                parserContext.module,
                methodQualifier,
                name,
                setf,
                paramTypes.toArray(Symbol[]::new),
                slotsAndNewContext.restArgsSlot != -1,
                rootNode);
    }

    ISLISPDefClassNode parseDefClass(ParserContext parserContext, Object sexpr) {
        var ctx = ISLISPContext.get(null);
        var args = requireList(sexpr, 4, -1);
        var className = downcast(args.get(1), Symbol.class);
        var parentClasses = new ArrayList<Symbol>();
        for (var e: requireList(args.get(2), -1, -1)) {
            parentClasses.add((Symbol) e);
        }
        var slots = new ArrayList<ISLISPDefClassNode.SlotDefinition>();
        for (var slot: requireList(args.get(3), -1, -1)) {
            var slotDef = new ISLISPDefClassNode.SlotDefinition();
            if (slot instanceof Symbol s) {
                slotDef.setName(s);
            } else {
                var slotDefLst = requireList(slot, 1, -1);
                slotDef.setName((Symbol) slotDefLst.get(0));
                var readers = new ArrayList<Symbol>();
                var writters = new ArrayList<Symbol>();
                var accessors = new ArrayList<Symbol>();
                var boundp = new ArrayList<Symbol>();
                Symbol initArg = null;
                ISLISPRootNode initForm = null;

                for (int i = 1; i < slotDefLst.size(); i += 2) {
                    var key = downcast(slotDefLst.get(i), Symbol.class);
                    var value = slotDefLst.get(i + 1);
                    switch (key.name()) {
                        case ":reader" -> readers.add(downcast(value, Symbol.class));
                        case ":writer" -> writters.add(downcast(value, Symbol.class));
                        case ":accessor" -> accessors.add(downcast(value, Symbol.class));
                        case ":boundp" -> boundp.add(downcast(value, Symbol.class));
                        case ":initform" -> {
                            if (initForm != null) {
                                throw new ParsingException(source(key), "Duplicate init form");
                            }
                            var newParserContext = parserContext.pushFrameDescriptor();
                            var formExpression = parseExpressionNode(newParserContext, value);
                            initForm = new ISLISPRootNode(
                                    ctx.getLanguage(),
                                    new ISLISPExpressionNode[]{formExpression},
                                    newParserContext.frameBuilder.build());
                        }
                        case ":initarg" -> {
                            if (initArg != null) {
                                throw new ParsingException(source(initArg), "Duplicate init arg");
                            }
                            initArg = (Symbol) value;
                        }
                        default -> throw new ParsingException(source(key), "Unknown defclass option");
                    }
                }
                slotDef.setInitArg(initArg);
                slotDef.setInitializer(initForm);
                slotDef.setAccessorName(accessors.toArray(Symbol[]::new));
                slotDef.setReaderName(readers.toArray(Symbol[]::new));
                slotDef.setWriterName(writters.toArray(Symbol[]::new));
                slotDef.setBoundpName(boundp.toArray(Symbol[]::new));
            }
            slots.add(slotDef);
        }
        var isAbstract = false;
        for (var e: args.subList(4, args.size())) {
            var opt = requireList(e, 2, 2);
            var key = downcast(opt.get(0), Symbol.class);
            if (key.name().equals(":abstractp")) {
                var value = downcast(opt.get(1), Symbol.class);
                isAbstract = !(value.identityReference().getId() == ctx.getNil().identityReference().getId());
            }
        }
        return new ISLISPDefClassNode(
            ctx.getLanguage(),
            parserContext.module,
            className,
            parentClasses,
            slots,
            isAbstract,
            source(sexpr));
    }

    ISLISPDefGenericNode parseDefGeneric(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 3);
        Symbol name;
        boolean setf;
        if (args.get(1) instanceof Symbol s) {
            name = s;
            setf = false;
        } else {
            var setfForm = requireList(args.get(1), 2, 2);
            if (!(setfForm.get(0) instanceof Symbol setfSymbol) || !setfSymbol.name().equals("setf")) {
                throw new ParsingException(source(sexpr), "Bad defgeneric function spec");
            }
            name = downcast(setfForm.get(1), Symbol.class);
            setf = true;
        }
        //TODO :rest
        var lambdaList = requireList(args.get(2), -1, -1);
        return new ISLISPDefGenericNode(parserContext.module, name, setf, lambdaList.size(), false, source(sexpr));
    }

    ISLISPReturnFromNode parseReturnFrom(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 3);
        var name = downcast(args.get(1), Symbol.class);
        var blockId = parserContext.blocks.get(name.identityReference())
                .orElseThrow(() -> new ParsingException(source(sexpr), "Bogus return-from"));
        var expression = parseExpressionNode(parserContext, args.get(2));
        return new ISLISPReturnFromNode(blockId, expression, source(sexpr));
    }

    ISLISPBlockNode parseBlock(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        var name = downcast(args.get(1), Symbol.class);
        var newContext = parserContext.pushBlockScope(name.identityReference());
        var blockId = newContext.blocks.get(name.identityReference())
                .orElseThrow(() -> new ParsingException(source(sexpr), "Should never happen"));
        ISLISPExpressionNode[] expressions = new ISLISPExpressionNode[args.size() - 2];
        for (int i = 2; i < args.size(); i++) {
            expressions[i - 2] = parseExpressionNode(newContext, args.get(i));
        }
        return new ISLISPBlockNode(blockId, expressions, source(sexpr));
    }

    ISLISPLiteralNode parseQuote(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, 2);
        return new ISLISPLiteralNode(args.get(1), source(sexpr));
    }

    ISLISPExpressionNode parseDebuggerNode(ParserContext parserContext, Object sexpr) {
        return new ISLISPDebuggerNode(source(sexpr));
    }

    ISLISPExpressionNode parseIndirectFunCall(ParserContext parserContext, Object sexpr, boolean lastArgRest) {
        var args = requireList(sexpr, 2, -1);
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        for (Object arg : args.subList(1, args.size())) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        return new ISLISPIndirectFunctionCallNode(
                argNodes.get(0),
                argNodes.subList(1, argNodes.size()).toArray(ISLISPExpressionNode[]::new),
                lastArgRest,
                source(sexpr));
    }

    ISLISPExpressionNode parseDirectSetfFunctionCall(
        ParserContext parserContext,
        Object sexpr,
        List<Object> placeList,
        Object value
    ) {
        var name = downcast(placeList.get(0), Symbol.class);
        var args = new ArrayList<ISLISPExpressionNode>();
        args.add(parseExpressionNode(parserContext, value));
        for (int i = 1; i < placeList.size(); i++) {
            args.add(parseExpressionNode(parserContext, placeList.get(i)));
        }
        return new ISLISPGlobalFunctionCallNode(
            parserContext.module,
            name,
            true,
            args.toArray(ISLISPExpressionNode[]::new),
            source(sexpr));
    }

    ISLISPExpressionNode parseDirectFunctionCall(
            ParserContext parserContext,
            Object sexpr
    ) {
        var args = requireList(sexpr, 1, -1);
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        var name = downcast(args.get(0), Symbol.class);
        for (Object arg : args.subList(1, args.size())) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        var maybeVar = parserContext.localFunctions.get(name.identityReference());
        if (maybeVar.isPresent()) {
            var variableContext = maybeVar.get();
            var index = parserContext.frameDepth - variableContext.frameDepth;
            var functionLookup = new ISLISPLexicalIdentifierNode(index, variableContext.slot, source(name));
            return new ISLISPIndirectFunctionCallNode(
                    functionLookup,
                    argNodes.toArray(ISLISPExpressionNode[]::new),
                    false,
                    source(sexpr));
        } else {
            return new ISLISPGlobalFunctionCallNode(
                parserContext.module,
                name,
                false,
                argNodes.toArray(ISLISPExpressionNode[]::new),
                source(sexpr));
        }
    }

    ISLISPDirectLambdaCallNode parseDirectLambdaCall(
            ParserContext parserContext,
            ISLISPLambdaNode lambdaNode,
            Object sexpr
    ) {
        var args = requireList(sexpr, 1, -1);
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        for (Object arg : args.subList(1, args.size())) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        return new ISLISPDirectLambdaCallNode(lambdaNode, argNodes.toArray(ISLISPExpressionNode[]::new), source(sexpr));
    }

    ISLISPLambdaNode makeLambdaNode(
        ParserContext parserContext,
        Object argList,
        List<Object> bodyExprs,
        SourceSection source
    ) {
        parserContext = parserContext.pushFrameDescriptor();
        var slotsAndNewContext =
            processFrameDescriptorsForFunctionArguments(parserContext.pushClosureScope(), argList);
        var bodyStatements = bodyExprs.stream()
            .map(v -> parseExpressionNode(slotsAndNewContext.context, v))
            .toArray(ISLISPExpressionNode[]::new);
        var body = new ISLISPPrognNode(
            bodyStatements,
            span(
                bodyStatements[0].getSourceSection(),
                bodyStatements[bodyStatements.length - 1].getSourceSection()));
        body.setParserContext(slotsAndNewContext.context);
        var ctx = ISLISPContext.get(null);
        var userDefinedFunctionNode = new ISLISPUserDefinedFunctionNode(
            ctx.getLanguage(),
            body,
            slotsAndNewContext.namedArgsSlots,
            slotsAndNewContext.restArgsSlot,
            -1,
            -1,
            source);
        var rootNode = new ISLISPRootNode(
            ctx.getLanguage(),
            new ISLISPExpressionNode[]{userDefinedFunctionNode},
            parserContext.frameBuilder.build());
        return new ISLISPLambdaNode(rootNode);
    }

    ISLISPLambdaNode parseLambda(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        return makeLambdaNode(parserContext, args.get(1), args.subList(2, args.size()), source(sexpr));
    }

    ISLISPDefunNode parseDefun(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, -1);
        var restList = args.subList(1, args.size());
        var name = downcast(restList.get(0), Symbol.class);
        parserContext = parserContext.pushFrameDescriptor();
        var slotsAndNewContext = processFrameDescriptorsForFunctionArguments(parserContext, restList.get(1));
        var bodyStatements = restList.stream()
                .skip(2)
                .map(v -> parseExpressionNode(slotsAndNewContext.context, v))
                .toArray(ISLISPExpressionNode[]::new);
        var body = new ISLISPPrognNode(
                bodyStatements,
                null);
        body.setParserContext(slotsAndNewContext.context);
        var ctx = ISLISPContext.get(null);
        var userDefinedFunctionNode = new ISLISPUserDefinedFunctionNode(
                ctx.getLanguage(),
                body,
                slotsAndNewContext.namedArgsSlots,
                slotsAndNewContext.restArgsSlot,
                -1,
                -1,
                source(sexpr));
        var rootNode = new ISLISPRootNode(
                ctx.getLanguage(),
                new ISLISPExpressionNode[]{userDefinedFunctionNode},
                parserContext.frameBuilder.build());
        return new ISLISPDefunNode(parserContext.module, name, rootNode);
    }

    SlotsAndNewContext processFrameDescriptorsForFunctionArguments(
            ParserContext parserContext,
            Object parameterList
    ) {
        var args = requireList(parameterList, -1, -1)
                .stream()
                .map(v -> downcast(v, Symbol.class))
                .collect(Collectors.toList());
        return processFrameDescriptorsForFunctionArguments(parserContext, args);
    }

    SlotsAndNewContext processFrameDescriptorsForFunctionArguments(
            ParserContext parserContext,
            List<Symbol> parameterList
    ) {
        var restSlot = -1;
        var positionalArgumentSlots = new ArrayList<Integer>();
        var variables = new HashMap<SymbolReference, ParserContext.VariableContext>();
        int stateNamedArgs = 0;
        int stateAfterRestKw = 1;
        int stateAfterRestArg = 2;
        int state = stateNamedArgs;
        for (var arg: parameterList) {
            var isRestKw = "&rest".equals(arg.name()) || ":rest".equals(arg.name());
            if (state == stateNamedArgs && !isRestKw) {
                var slot = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
                positionalArgumentSlots.add(slot);
                var variableContext = new ParserContext.VariableContext();
                variableContext.frameDepth = parserContext.frameDepth;
                variableContext.slot = slot;
                variableContext.name = arg.name();
                variables.put(arg.identityReference(), variableContext);
            } else if (state == stateNamedArgs && isRestKw) {
                state = stateAfterRestKw;
            } else if (state == stateAfterRestKw && !isRestKw) {
                state = stateAfterRestArg;
                restSlot = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
                var variableContext = new ParserContext.VariableContext();
                variableContext.frameDepth = parserContext.frameDepth;
                variableContext.slot = restSlot;
                variableContext.name = arg.name();
                variables.put(arg.identityReference(), variableContext);
            } else if (state == stateAfterRestArg) {
                throw new ParsingException(source(arg), "Multiple symbols after :rest");
            } else {
                throw new ParsingException(source(arg), "Bad parameter list form");
            }
        }
        var argSlots = new int[positionalArgumentSlots.size()];
        for (var i = 0; i < positionalArgumentSlots.size(); i++) {
            argSlots[i] = positionalArgumentSlots.get(i);
        }
        var newContext = parserContext.pushLexicalScope(variables);
        var result = new SlotsAndNewContext();
        result.namedArgsSlots = argSlots;
        result.context = newContext;
        result.restArgsSlot = restSlot;
        return result;
    }

    ISLISPPrognNode parseProgn(ParserContext parserContext, Object sexpr, boolean isTopLevel)  {
        var args = requireList(sexpr, 1, -1);
        var bodyStatements = new ArrayList<ISLISPExpressionNode>();
        for (var e: args.subList(1, args.size())) {
            bodyStatements.add(parseExpressionNode(parserContext, e, isTopLevel));
        }
        return new ISLISPPrognNode(bodyStatements.toArray(ISLISPExpressionNode[]::new), source(sexpr));
    }

    ISLISPExpressionNode parseFunctionRef(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, 2);
        var name = downcast(args.get(1), Symbol.class);
        var maybeVar = parserContext.localFunctions.get(name.identityReference());
        if (maybeVar.isPresent()) {
            var variableContext = maybeVar.get();
            var index = parserContext.frameDepth - variableContext.frameDepth;
            return new ISLISPLexicalIdentifierNode(index, variableContext.slot, source(sexpr));
        } else {
            return new ISLISPFunctionRefNode(parserContext.module, name, source(sexpr));
        }
    }

    ISLISPClassRefNode parseClassRef(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, 2);
        var name = downcast(args.get(1), Symbol.class);
        return new ISLISPClassRefNode(parserContext.module, name, source(sexpr));
    }

    ISLISPIfNode parseIfNode(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 4);
        var body = new ArrayList<ISLISPExpressionNode>();
        for (var e: args.subList(1, args.size())) {
            body.add(parseExpressionNode(parserContext, e));
        }
        var test = body.get(0);
        var truthy = body.get(1);
        var falsy = body.size() == 2 ? new ISLISPLiteralNode(ISLISPContext.get(null).getNil(), null) : body.get(2);
        return new ISLISPIfNode(test, truthy, falsy, source(sexpr));
    }

    ISLISPDefMacroNode parseDefMacro(ParserContext parserContext, Object sexpr) {
        return new ISLISPDefMacroNode(parseDefun(parserContext, sexpr));
    }

    ISLISPQuasiquoteNode parseQuasiquote(ParserContext parserContext, Object sexpr) {
        var qq = QuasiquoteTree.parseQuasiquoteTree(sexpr);
        var childNodes = new ISLISPExpressionNode[qq.expressions().length];
        for (var i = 0; i < childNodes.length; i++) {
            childNodes[i] = parseExpressionNode(parserContext, qq.expressions()[i]);
        }
        return new ISLISPQuasiquoteNode(source(sexpr), qq.tree(), childNodes);
    }

    ISLISPLetNode parseLetNode(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        var variablesList = requireList(args.get(1), -1, -1);
        var bodyExpressions = args.subList(2, args.size());
        var variableSlots = new int[variablesList.size()];
        var variableInitializers = new ISLISPExpressionNode[variablesList.size()];
        var variableNameMap = new HashMap<SymbolReference, ParserContext.VariableContext>();
        for (var i = 0; i < variablesList.size(); i++) {
            var variable = requireList(variablesList.get(i), 2, 2);
            var variableName = downcast(variable.get(0), Symbol.class);
            var variableInitializer = variable.get(1);
            variableSlots[i] = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
            variableInitializers[i] = parseExpressionNode(parserContext, variableInitializer);
            if (variableNameMap.containsKey(variableName.identityReference())) {
                throw new ParsingException(source(sexpr), "Duplicate variable declaration in let");
            }
            var variableContext = new ParserContext.VariableContext();
            variableContext.slot = variableSlots[i];
            variableContext.frameDepth = parserContext.frameDepth;
            variableContext.name = variableName.name();
            variableNameMap.put(variableName.identityReference(), variableContext);
        }
        parserContext = parserContext.pushLexicalScope(variableNameMap);
        var body = new ISLISPExpressionNode[bodyExpressions.size()];
        for (int i = 0; i < bodyExpressions.size(); i++) {
            body[i] = parseExpressionNode(parserContext, bodyExpressions.get(i));
            body[i].setParserContext(parserContext);
        }
        return new ISLISPLetNode(variableSlots, variableInitializers, body, source(sexpr));
    }

    ISLISPLetNode parseLetStarNode(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        var variablesList = requireList(args.get(1), -1, -1);
        var bodyExpressions = args.subList(2, args.size());
        var variableSlots = new int[variablesList.size()];
        var variableInitializers = new ISLISPExpressionNode[variablesList.size()];
        for (var i = 0; i < variablesList.size(); i++) {
            var variable = requireList(variablesList.get(i), 2, 2);
            var variableName = downcast(variable.get(0), Symbol.class);
            var variableInitializer = variable.get(1);
            variableSlots[i] = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
            variableInitializers[i] = parseExpressionNode(parserContext, variableInitializer);
            variableInitializers[i].setParserContext(parserContext);
            var variableContext = new ParserContext.VariableContext();
            variableContext.slot = variableSlots[i];
            variableContext.frameDepth = parserContext.frameDepth;
            variableContext.name = variableName.name();
            parserContext = parserContext.pushLexicalScope(Map.of(variableName.identityReference(), variableContext));
        }
        var body = new ISLISPExpressionNode[bodyExpressions.size()];
        for (int i = 0; i < bodyExpressions.size(); i++) {
            body[i] = parseExpressionNode(parserContext, bodyExpressions.get(i));
        }
        return new ISLISPLetNode(variableSlots, variableInitializers, body, source(sexpr));
    }

    ISLISPLetNode parseFletNode(ParserContext parserContext, Object sexpr) {
        return parseFunctionLetNode(parserContext, sexpr, false);
    }

    ISLISPLetNode parseLabelsNode(ParserContext parserContext, Object sexpr) {
        return parseFunctionLetNode(parserContext, sexpr, true);
    }

    ISLISPLetNode parseFunctionLetNode(ParserContext parserContext, Object sexpr, boolean augmentFunctionScope) {
        var args = requireList(sexpr, 2, -1);
        var functionList = requireList(args.get(1), -1, -1);
        var bindingSlots = new int[functionList.size()];
        var lambdaInitializers = new ISLISPExpressionNode[functionList.size()];
        var bindingNameMap = new HashMap<SymbolReference, ParserContext.VariableContext>();
        for (int i = 0; i < functionList.size(); i++) {
            var function = requireList(functionList.get(i), 3, -1);
            var name = downcast(function.get(0), Symbol.class);
            bindingSlots[i] = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
            var variableContext = new ParserContext.VariableContext();
            variableContext.slot = bindingSlots[i];
            variableContext.frameDepth = parserContext.frameDepth;
            variableContext.name = name.name();
            if (bindingNameMap.containsKey(name.identityReference())) {
                throw new ParsingException(source(sexpr), "Duplicate variable declaration.");
            }
            bindingNameMap.put(name.identityReference(), variableContext);
        }
        var augmentedParserContext = parserContext.pushLexicalFunctionScope(bindingNameMap);
        var lambdaBodyContext = augmentFunctionScope
            ? augmentedParserContext
            : parserContext;
        for (int i = 0; i < functionList.size(); i++) {
            var function = requireList(functionList.get(i), 3, -1);
            var argumentList = function.get(1);
            var body = function.subList(2, function.size());
            lambdaInitializers[i] = makeLambdaNode(lambdaBodyContext, argumentList, body, source(function));
        }
        var body = args
            .stream()
            .skip(2)
            .map(e -> parseExpressionNode(augmentedParserContext, e))
            .toArray(ISLISPExpressionNode[]::new);
        return new ISLISPLetNode(bindingSlots, lambdaInitializers, body, source(sexpr));
    }

    ISLISPUnwindProtectNode parseUnwindProtectNode(
            ParserContext parserContext,
            Object sexpr
    ) {
        var args = requireList(sexpr, 2, -1);
        var body = parseExpressionNode(parserContext, args.get(1));
        var cleanups = new ISLISPExpressionNode[args.size() - 2];
        for (int i = 0; i < cleanups.length; i++) {
            cleanups[i] = parseExpressionNode(parserContext, args.get(i + 2));
        }
        return new ISLISPUnwindProtectNode(body, cleanups, source(sexpr));
    }

    <T> T downcast(Object value, Class<T> clazz) throws ParsingException {
        if (clazz.isAssignableFrom(value.getClass())) {
            return (T) value;
        }
        throw new ParsingException(
            source(value),
            "Expected " + clazz.getSimpleName() + "; was " + value.getClass().getSimpleName() + ".");
    }

    List<Object> requireList(Object value, int minLength, int maxLength) throws ParsingException {
        if (value instanceof Pair || (value instanceof Symbol s && s.name().equals("nil"))) {
            var lst = Utils.readList(value);
            if (minLength >= 0 && minLength == maxLength && lst.size() != minLength) {
                throw new ParsingException(
                    source(value),
                    "Expected a list of length " + minLength + "; was " + lst.size() + ".");
            }
            if (minLength >= 0 && lst.size() < minLength) {
                throw new ParsingException(
                    source(value),
                    "Expected a list of at least length " + minLength + "; was " + lst.size() + ".");
            }
            if (maxLength >= 0 && lst.size() > maxLength) {
                throw new ParsingException(
                    source(value),
                    "Expected a list of at most length " + minLength + "; was " + lst.size() + ".");
            }
            return lst;
        }
        throw new ParsingException(
            source(value),
            "Expected a list; was " + value.getClass().getSimpleName() + ".");
    }

    SourceSection source(Object sexpr) {
        return sourceSectionMap.get(new EqWrapper(sexpr));
    }

    /**
     * Loads all given modules (and all their transitive dependencies).
     *
     * @param requires list of modules to load
     */
    public void ensureRequiresLoaded(List<String> requires) {
        var ctx = ISLISPContext.get(null);
        for (var req: requires) {
            if (ctx.getModule(req) == null) {
                if (moduleLoadInProgress.contains(req)) {
                    throw new RuntimeException("Cyclical import detected");
                }
                moduleLoadInProgress.add(req);
                loadModule(req);
                moduleLoadInProgress.remove(req);
            }
        }
    }

    void loadModule(String module) {
        try {
            var ctx = ISLISPContext.get(null);
            var file = locateModuleSourceFile(module);
            var source = Source.newBuilder("islisp", file).build();
            var moduleSource = parseModuleSource(module, source);
            ensureRequiresLoaded(moduleSource.requires());
            ctx.createModule(module, moduleSource.requires(), moduleSource.provides());
            new ISLISPRootNode(
                ctx.getLanguage(),
                new ISLISPExpressionNode[]{new ISLISPModuleNode(this, moduleSource)},
                null
            ).getCallTarget().call();
        } catch (IOException e) {
            throw new RuntimeException("Failed to load module source", e);
        }
    }

    TruffleFile locateModuleSourceFile(String module) {
        var env = ISLISPContext.get(null).getEnv();
        for (var rootPath: env.getOptions().get(ISLISPTruffleLanguage.Sourcepath).split(":")) {
            var root = env.getPublicTruffleFile(rootPath);
            var resolved = root.resolve(module);
            if (resolved.exists()) {
                return resolved;
            }
        }
        throw new RuntimeException("Failed to find module source");
    }

    static class SlotsAndNewContext {
        int[] namedArgsSlots;
        int restArgsSlot;
        ParserContext context;
    }
}
