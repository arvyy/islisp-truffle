package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.nodes.*;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.source.SourceSection;

import java.util.*;
import java.util.stream.Collectors;

public class Parser {

    private final Map<EqWrapper, SourceSection> sourceSectionMap;

    public Parser(Map<EqWrapper, SourceSection> sourceSectionMap) {
        this.sourceSectionMap = sourceSectionMap;
    }

    public ISLISPRootNode parseRootNode(ISLISPTruffleLanguage language, List<Object> content) {
        var expressionNodes = new ArrayList<ISLISPExpressionNode>();
        var parserContext = new ParserContext();
        for (var v: content) {
            var expression = parseExpressionNode(parserContext, v, true);
            executeDefinitions(expression); // execute definitions to be available for macro procedure runs
            filterMacros(expression).ifPresent(expressionNodes::add);
        }
        var ctx = ISLISPContext.get(null);
        ctx.reset();
        var root = new ISLISPRootNode(
                language,
                expressionNodes.toArray(ISLISPExpressionNode[]::new),
                parserContext.frameBuilder.build()
        );
        return root;
    }

    SourceSection span(SourceSection a, SourceSection b) {
        return a.getSource().createSection(
                a.getStartLine(), a.getStartColumn(),
                b.getEndLine(), b.getEndColumn()
        );
    }

    void executeDefinitions(ISLISPExpressionNode expression) {
        if (expression.isDefinitionNode()) {
            var root = new ISLISPRootNode(
                    null,
                    new ISLISPExpressionNode[]{expression},
                    null);
            root.getCallTarget().call();
        }
        if (expression instanceof ISLISPPrognNode) {
            for (var e: ((ISLISPPrognNode) expression).getBodyNodes()) {
                executeDefinitions(e);
            }
        }
    }

    Optional<ISLISPExpressionNode> filterMacros(ISLISPExpressionNode expression) {
        if (expression instanceof ISLISPDefMacroNode) {
            return Optional.empty();
        }
        if (expression instanceof ISLISPPrognNode) {
            var exprs = new ArrayList<ISLISPExpressionNode>();
            for (var e: ((ISLISPPrognNode) expression).getBodyNodes()) {
                filterMacros(e).ifPresent(exprs::add);
            }
            return Optional.of(
                    new ISLISPPrognNode(
                            exprs.toArray(ISLISPExpressionNode[]::new),
                            expression.getSourceSection()));
        }
        return Optional.of(expression);
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
                //TODO create node
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
                case "progn":
                    return parseProgn(parserContext, sexpr, topLevel);
                case "funcall":
                    return parseIndirectFunCall(parserContext, sexpr);
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
                case "block":
                    return parseBlock(parserContext, sexpr);
                case "return-from":
                    return parseReturnFrom(parserContext, sexpr);
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
                case "with-handler":
                    return parseWithHandler(parserContext, sexpr);

                default:
            }
            // macros
            if ("setf".equals(carName)) {
                var args = requireList(form, 3, 3);
                var place = macroExpand(args.get(1), false);
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
                        .lookupSetfTransformer(setfDispatchSymbol.identityReference());
                if (setfDispatch == null) {
                    return parseDirectSetfFunctionCall(parserContext, sexpr, placeList, value);
                }
                var transformed = setfDispatch.transform(placeList, value);
                sourceSectionMap.put(new EqWrapper(transformed), source(sexpr));
                return parseExpressionNode(
                        parserContext,
                        transformed);
            }
            var expanded = macroExpand(sexpr, true);
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
            || sexpr instanceof LispChar
            || sexpr instanceof String
            || sexpr instanceof LispVector
        ) {
            return new ISLISPLiteralNode(sexpr, null);
        }
        if (sexpr instanceof Symbol symbol) {
            var maybeLexicalSlot = parserContext.variables.get(symbol.identityReference());
            if (maybeLexicalSlot.isPresent()) {
                var variableContext = maybeLexicalSlot.get();
                var index = parserContext.frameDepth - variableContext.frameDepth;
                var slot = variableContext.slot;
                return new ISLISPLexicalIdentifierNode(index, slot, source(sexpr));
            } else {
                return new ISLISPGlobalIdentifierNode(symbol, source(sexpr));
            }
        }
        throw new ParsingException(source(sexpr), "Unrecognized form.");
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
        return new ISLISPDefGlobalNode(name, init, source(sexpr));
    }

    ISLISPDefConstantNode parseDefConstant(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 3);
        var name = downcast(args.get(1), Symbol.class);
        var init = parseExpressionNode(parserContext, args.get(2));
        return new ISLISPDefConstantNode(name, init, source(sexpr));
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
            return new ISLISPSetqNode(index, variableContext.slot, expr, source(sexpr));
        } else {
            return new ISLISPSetqNode(name, expr, source(sexpr));
        }
    }

    Object macroExpand(Object form, boolean single) {
        if (form instanceof Pair p && p.car() instanceof Symbol symbol) {
            var rest = p.cdr();
            var maybeMacro = ISLISPContext.get(null).lookupMacro(symbol.identityReference());
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
                            .map(part -> macroExpand(part, false))
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
        return new ISLISPSetDynamicNode(symbol, initalizer, source(sexpr));
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
        return new ISLISPDynamicLetNode(symbols, initializers, body, source(sexpr));
    }

    ISLISPDynamicLookupNode parseDynamic(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, 2);
        var name = downcast(args.get(1), Symbol.class);
        return new ISLISPDynamicLookupNode(name, source(sexpr));
    }

    ISLISPDefDynamicNode parseDefDynamic(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 3, 3);
        return new ISLISPDefDynamicNode(
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
        var paramTypes = new ArrayList<Symbol>();
        for (var el: parameterList) {
            if (el instanceof Symbol s) {
                plainParamList.add(s);
                paramTypes.add(ISLISPContext.get(null).namedSymbol("<object>"));
            } else if (el instanceof Pair p) {
                var paramWithType = requireList(p, 2, 2);
                plainParamList.add(downcast(paramWithType.get(0), Symbol.class));
                paramTypes.add(downcast(paramWithType.get(1), Symbol.class));
            }
        }
        parserContext = parserContext.pushFrameDescriptor();
        var slotsAndNewContext = processFrameDescriptorsForFunctionArguments(parserContext, plainParamList);
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
            var nextMethodPVar = new ParserContext.VariableContext();
            nextMethodPVar.slot = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
            nextMethodPVar.frameDepth = 0;
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
                methodQualifier,
                name,
                setf,
                paramTypes.toArray(Symbol[]::new),
                slotsAndNewContext.namedArgsSlots.length,
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
        return new ISLISPDefClassNode(ctx.getLanguage(), className, parentClasses, slots, isAbstract, source(sexpr));
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
        return new ISLISPDefGenericNode(name, setf, lambdaList.size(), false, source(sexpr));
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

    ISLISPExpressionNode parseIndirectFunCall(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        for (Object arg : args.subList(1, args.size())) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        return new ISLISPIndirectFunctionCallNode(
                argNodes.get(0),
                argNodes.subList(1, argNodes.size()).toArray(ISLISPExpressionNode[]::new),
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
        return new ISLISPGlobalFunctionCallNode(name, true, args.toArray(ISLISPExpressionNode[]::new), source(sexpr));
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
                    source(sexpr));
        } else {
            return new ISLISPGlobalFunctionCallNode(name, false, argNodes.toArray(ISLISPExpressionNode[]::new), source(sexpr));
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

    ISLISPLambdaNode parseLambda(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, -1);
        var restList = args.subList(1, args.size());
        parserContext = parserContext.pushFrameDescriptor();
        var slotsAndNewContext =
                processFrameDescriptorsForFunctionArguments(parserContext.pushClosureScope(), restList.get(0));
        var bodyStatements = restList.stream()
                .skip(1)
                .map(v -> parseExpressionNode(slotsAndNewContext.context, v))
                .toArray(ISLISPExpressionNode[]::new);
        var body = new ISLISPPrognNode(
                bodyStatements,
                span(
                        bodyStatements[0].getSourceSection(),
                        bodyStatements[bodyStatements.length - 1].getSourceSection()));
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
        return new ISLISPLambdaNode(rootNode);
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
        return new ISLISPDefunNode(name, rootNode);
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
                variables.put(arg.identityReference(), variableContext);
            } else if (state == stateNamedArgs && isRestKw) {
                state = stateAfterRestKw;
            } else if (state == stateAfterRestKw && !isRestKw) {
                state = stateAfterRestArg;
                restSlot = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
                var variableContext = new ParserContext.VariableContext();
                variableContext.frameDepth = parserContext.frameDepth;
                variableContext.slot = restSlot;
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
            return new ISLISPFunctionRefNode(name, source(sexpr));
        }
    }

    ISLISPClassRefNode parseClassRef(ParserContext parserContext, Object sexpr) {
        var args = requireList(sexpr, 2, 2);
        var name = downcast(args.get(1), Symbol.class);
        return new ISLISPClassRefNode(name, source(sexpr));
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
            variableNameMap.put(variableName.identityReference(), variableContext);
        }
        parserContext = parserContext.pushLexicalScope(variableNameMap);
        var body = new ISLISPExpressionNode[bodyExpressions.size()];
        for (int i = 0; i < bodyExpressions.size(); i++) {
            body[i] = parseExpressionNode(parserContext, bodyExpressions.get(i));
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
            var variableContext = new ParserContext.VariableContext();
            variableContext.slot = variableSlots[i];
            variableContext.frameDepth = parserContext.frameDepth;
            parserContext = parserContext.pushLexicalScope(Map.of(variableName.identityReference(), variableContext));
        }
        var body = new ISLISPExpressionNode[bodyExpressions.size()];
        for (int i = 0; i < bodyExpressions.size(); i++) {
            body[i] = parseExpressionNode(parserContext, bodyExpressions.get(i));
        }
        return new ISLISPLetNode(variableSlots, variableInitializers, body, source(sexpr));
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

     static class SlotsAndNewContext {
        int[] namedArgsSlots;
        int restArgsSlot;
        ParserContext context;
    }
}
