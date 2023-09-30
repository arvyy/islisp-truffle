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

    public ISLISPRootNode parseRootNode(ISLISPTruffleLanguage language, List<Value> content) {
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
                parserContext.frameBuilder.build(),
                span(
                        expressionNodes.get(0).getSourceSection(),
                        expressionNodes.get(expressionNodes.size() - 1).getSourceSection())
        );
        return root;
    }

    SourceSection span(SourceSection a, SourceSection b) {
        return a.getSource().createSection(
                a.getStartLine(), a.getStartColumn(),
                b.getStartLine(), b.getStartColumn()
        );
    }

    void executeDefinitions(ISLISPExpressionNode expression) {
        if (expression.isDefinitionNode()) {
            var root = new ISLISPRootNode(
                    null,
                    new ISLISPExpressionNode[]{expression},
                    null,
                    expression.getSourceSection());
            root.getCallTarget().call();
        }
        if (expression instanceof ISLISPProgn) {
            for (var e: ((ISLISPProgn) expression).getBodyNodes()) {
                executeDefinitions(e);
            }
        }
    }

    Optional<ISLISPExpressionNode> filterMacros(ISLISPExpressionNode expression) {
        if (expression instanceof ISLISPDefMacro) {
            return Optional.empty();
        }
        if (expression instanceof ISLISPProgn) {
            var exprs = new ArrayList<ISLISPExpressionNode>();
            for (var e: ((ISLISPProgn) expression).getBodyNodes()) {
                filterMacros(e).ifPresent(exprs::add);
            }
            return Optional.of(
                    new ISLISPProgn(
                            exprs.toArray(ISLISPExpressionNode[]::new),
                            expression.getSourceSection()));
        }
        return Optional.of(expression);
    }

    ISLISPExpressionNode parseExpressionNode(ParserContext parserContext, Value sexpr) {
        return parseExpressionNode(parserContext, sexpr, false);
    }

    ISLISPExpressionNode parseExpressionNode(ParserContext parserContext, Value sexpr, boolean topLevel) {
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
                                sexpr.sourceSection(),
                                String.format("%s required to be at top level.", carName));
                    default:
                }
            }
            // builtins
            switch (carName) {
                //TODO create node
                case "defclass":
                    return parseDefClass(parserContext, sexpr.sourceSection(), sexpr);
                case "defconstant":
                    return parseDefConstant(parserContext, sexpr.sourceSection(), sexpr);
                case "defdynamic":
                    return parseDefDynamic(parserContext, sexpr.sourceSection(), sexpr);
                case "defgeneric":
                    return parseDefGeneric(parserContext, sexpr.sourceSection(), sexpr);
                case "defglobal":
                    return parseDefGlobal(parserContext, sexpr.sourceSection(), sexpr);
                case "defmacro":
                    return parseDefMacro(parserContext, sexpr.sourceSection(), sexpr);
                case "defun":
                    return parseDefun(parserContext, sexpr.sourceSection(), sexpr);
                case "defmethod":
                    return parseDefMethod(parserContext, sexpr.sourceSection(), sexpr);
                case "catch":
                    return parseCatchNode(parserContext, sexpr.sourceSection(), sexpr);
                case "throw":
                    return parseThrowNode(parserContext, sexpr.sourceSection(), sexpr);
                case "progn":
                    return parseProgn(parserContext, sexpr.sourceSection(), sexpr, topLevel);
                case "funcall":
                    return parseIndirectFunCall(parserContext, sexpr.sourceSection(), sexpr);
                case "function":
                    return parseFunctionRef(parserContext, sexpr.sourceSection(), sexpr);
                case "lambda":
                    return parseLambda(parserContext, sexpr.sourceSection(), sexpr);
                case "if":
                    return parseIfNode(parserContext, sexpr.sourceSection(), sexpr);
                case "debugger":
                    return parseDebuggerNode(parserContext, sexpr.sourceSection(), sexpr);
                case "quote":
                    return parseQuote(parserContext, sexpr.sourceSection(), sexpr);
                case "quasiquote":
                    return parseQuasiquote(parserContext, sexpr.sourceSection(), sexpr);
                case "class":
                    return parseClassRef(parserContext, sexpr.sourceSection(), sexpr);
                case "block":
                    return parseBlock(parserContext, sexpr.sourceSection(), sexpr);
                case "return-from":
                    return parseReturnFrom(parserContext, sexpr.sourceSection(), sexpr);
                case "let":
                    return parseLetNode(parserContext, sexpr.sourceSection(), sexpr);
                case "let*":
                    return parseLetStarNode(parserContext, sexpr.sourceSection(), sexpr);
                case "dynamic":
                    return parseDynamic(parserContext, sexpr.sourceSection(), sexpr);
                case "dynamic-let":
                    return parseDynamicLet(parserContext, sexpr.sourceSection(), sexpr);
                case "set-dynamic":
                    return parseSetDynamic(parserContext, sexpr.sourceSection(), sexpr);
                case "setq":
                    return parseSetq(parserContext, sexpr.sourceSection(), sexpr);
                case "tagbody":
                    return parseTagBody(parserContext, sexpr.sourceSection(), sexpr);
                case "go":
                    return parseTagBodyGo(parserContext, sexpr.sourceSection(), sexpr);
                case "unwind-protect":
                    return parseUnwindProtectNode(parserContext, sexpr.sourceSection(), sexpr);
                default:
            }
            // macros
            if ("setf".equals(carName)) {
                var args = requireList(form, 3, 3);
                var place = macroExpand(args.get(1), false);
                var value = args.get(2);
                if (place instanceof Symbol s) {
                    return parseExpressionNode(parserContext, Utils.listToValue(List.of(
                            ISLISPContext.get(null).namedSymbol("setq"),
                            s,
                            value
                    ), sexpr.sourceSection()));
                }
                var placeList = requireList(place, 1, -1);
                var setfDispatchSymbol = downcast(placeList.get(0), Symbol.class);
                var setfDispatch = ISLISPContext.get(null)
                        .lookupSetfTransformer(setfDispatchSymbol.identityReference());
                if (setfDispatch == null) {
                    throw new ParsingException(form.sourceSection(), "Unknown setf form.");
                }
                return parseExpressionNode(
                        parserContext,
                        setfDispatch.transform(placeList, value, sexpr.sourceSection()));
            }
            var expanded = macroExpand(sexpr, true);
            if (sexpr == expanded) {
                return parseDirectFunctionCall(parserContext, sexpr.sourceSection(), sexpr);
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
                        var lambda = parseLambda(parserContext, firstPos.sourceSection(), firstPos);
                        return parseDirectLambdaCall(
                                parserContext,
                                sexpr.sourceSection(),
                                lambda,
                                sexpr);
                    }
                }
            }
        }
        if (sexpr instanceof LispInteger || sexpr instanceof LispChar) {
            return new ISLISPLiteralNode(sexpr, sexpr.sourceSection());
        }
        if (sexpr instanceof Symbol symbol) {
            var maybeLexicalSlot = parserContext.variables.get(symbol.identityReference());
            if (maybeLexicalSlot.isPresent()) {
                var variableContext = maybeLexicalSlot.get();
                var index = parserContext.frameDepth - variableContext.frameDepth;
                var slot = variableContext.slot;
                return new ISLISPLexicalIdentifierNode(index, slot, sexpr.sourceSection());
            } else {
                return new ISLISPGlobalIdentifierNode(symbol, sexpr.sourceSection());
            }
        }
        throw new ParsingException(sexpr.sourceSection(), "Unrecognized form.");
    }

    private ISLISPThrowNode parseThrowNode(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 3, 3);
        var tagForm = parseExpressionNode(parserContext, args.get(1));
        var resultForm = parseExpressionNode(parserContext, args.get(2));
        return new ISLISPThrowNode(tagForm, resultForm, sourceSection);
    }

    ISLISPCatchNode parseCatchNode(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 2, -1);
        var tagForm = parseExpressionNode(parserContext, args.get(1));
        var forms = args.stream()
                .skip(2)
                .map(v -> parseExpressionNode(parserContext, v))
                .toArray(ISLISPExpressionNode[]::new);
        return new ISLISPCatchNode(tagForm, forms, sourceSection);
    }

    ISLISPDefGlobalNode parseDefGlobal(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 3, 3);
        var name = downcast(args.get(1), Symbol.class);
        var init = parseExpressionNode(parserContext, args.get(2));
        return new ISLISPDefGlobalNode(name, init, sourceSection);
    }

    ISLISPDefConstantNode parseDefConstant(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 3, 3);
        var name = downcast(args.get(1), Symbol.class);
        var init = parseExpressionNode(parserContext, args.get(2));
        return new ISLISPDefConstantNode(name, init, sourceSection);
    }

    ISLISPTagBodyGoNode parseTagBodyGo(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 2, 2);
        var tagSymbol = downcast(args.get(1), Symbol.class);
        var maybeTagId = parserContext.tagbodyTags.get(tagSymbol.identityReference());
        if (maybeTagId.isPresent()) {
            return new ISLISPTagBodyGoNode(maybeTagId.get(), sourceSection);
        }
        throw new ParsingException(sourceSection, "Not found tag " + tagSymbol);
    }

    ISLISPTagBodyNode parseTagBody(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 1, -1);
        var expressions = new ArrayList<Value>();
        var tags = new HashMap<SymbolReference, Integer>();
        var tagSymbols = new ArrayList<SymbolReference>();
        for (var arg: args.subList(1, args.size())) {
            if (arg instanceof Symbol tag) {
                tags.put(tag.identityReference(), expressions.size());
                tagSymbols.add(tag.identityReference());
            } else if (arg instanceof Pair) {
                expressions.add(arg);
            } else {
                throw new ParsingException(arg.sourceSection(), "Tagbody part neither a symbol nor a complex expression.");
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
        return new ISLISPTagBodyNode(tagIds, tagPosition, parsedExpressions, sourceSection);
    }

    ISLISPSetqNode parseSetq(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 3, 3);
        var name = downcast(args.get(1), Symbol.class);
        var expr = parseExpressionNode(parserContext, args.get(2));
        var maybeVar = parserContext.variables.get(name.identityReference());
        if (maybeVar.isPresent()) {
            var variableContext = maybeVar.get();
            var index = parserContext.frameDepth - variableContext.frameDepth;
            return new ISLISPSetqNode(index, variableContext.slot, expr, sourceSection);
        } else {
            return new ISLISPSetqNode(name, expr, sourceSection);
        }
    }

    Value macroExpand(Value form, boolean single) {
        if (form instanceof Pair p && p.car() instanceof Symbol symbol) {
            var rest = p.cdr();
            var maybeMacro = ISLISPContext.get(null).lookupMacro(symbol.identityReference());
            if (maybeMacro != null) {
                var args = new ArrayList<Value>();
                args.add(null); // closure param
                Iterable<Value> it = rest.equals(ISLISPContext.get(null).getNil()) ? List.of() : (Pair) rest;
                for (var e: it) {
                    args.add(e);
                }
                var transformedSexpr = (Value) maybeMacro.callTarget().call(args.toArray());
                if (transformedSexpr instanceof Pair tp && !single) {
                    var parts = Utils.readList(tp);
                    var newParts = parts.stream()
                            .map(part -> macroExpand(part, false))
                            .collect(Collectors.toList());
                    return Utils.listToValue(newParts, form.sourceSection());
                } else {
                    return transformedSexpr;
                }
            }
        }
        return form;
    }

    ISLISPSetDynamicNode parseSetDynamic(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 3, 3);
        var initalizer = parseExpressionNode(parserContext, args.get(1));
        var symbol = (Symbol) args.get(2);
        return new ISLISPSetDynamicNode(symbol, initalizer, sourceSection);
    }

    ISLISPDynamicLetNode parseDynamicLet(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
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
        return new ISLISPDynamicLetNode(symbols, initializers, body, sourceSection);
    }

    ISLISPDynamicLookupNode parseDynamic(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 2, 2);
        var name = downcast(args.get(1), Symbol.class);
        return new ISLISPDynamicLookupNode(name, sourceSection);
    }

    ISLISPDefDynamicNode parseDefDynamic(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 3, 3);
        return new ISLISPDefDynamicNode(
                downcast(args.get(1), Symbol.class),
                parseExpressionNode(parserContext, args.get(2)), sourceSection);
    }

    ISLISPDefMethodNode parseDefMethod(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 3, -1);
        var name = (Symbol) args.get(1);
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
                throw new ParsingException(sourceSection, "Incompatible method qualifiers");
            }
            methodQualifier = ISLISPDefMethodNode.MethodQualifier.before;
        }
        if (methodQualifiers.contains(":after")) {
            if (methodQualifier != ISLISPDefMethodNode.MethodQualifier.none) {
                throw new ParsingException(sourceSection, "Incompatible method qualifiers");
            }
            methodQualifier = ISLISPDefMethodNode.MethodQualifier.after;
        }
        if (methodQualifiers.contains(":around")) {
            if (methodQualifier != ISLISPDefMethodNode.MethodQualifier.none) {
                throw new ParsingException(sourceSection, "Incompatible method qualifiers");
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
        var body = new ISLISPProgn(
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
                sourceSection);
        var rootNode = new ISLISPRootNode(
                ctx.getLanguage(),
                new ISLISPExpressionNode[]{userDefinedFunctionNode},
                parserContext.frameBuilder.build(),
                sourceSection);
        return new ISLISPDefMethodNode(
                methodQualifier,
                name,
                paramTypes.toArray(Symbol[]::new),
                slotsAndNewContext.namedArgsSlots.length,
                slotsAndNewContext.restArgsSlot != -1,
                rootNode);
    }

    ISLISPDefClassNode parseDefClass(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
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
                                throw new ParsingException(key.sourceSection(), "Duplicate init form");
                            }
                            var newParserContext = parserContext.pushFrameDescriptor();
                            var formExpression = parseExpressionNode(newParserContext, value);
                            initForm = new ISLISPRootNode(
                                    ctx.getLanguage(),
                                    new ISLISPExpressionNode[]{formExpression},
                                    newParserContext.frameBuilder.build(),
                                    value.sourceSection());
                        }
                        case ":initarg" -> {
                            if (initArg != null) {
                                throw new ParsingException(initArg.sourceSection(), "Duplicate init arg");
                            }
                            initArg = (Symbol) value;
                        }
                        default -> throw new ParsingException(key.sourceSection(), "Unknown defclass option");
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
        return new ISLISPDefClassNode(ctx.getLanguage(), className, parentClasses, slots, isAbstract, sourceSection);
    }

    ISLISPDefGeneric parseDefGeneric(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 3, 3);
        var name = downcast(args.get(1), Symbol.class);
        //TODO :rest
        var lambdaList = requireList(args.get(2), -1, -1);
        return new ISLISPDefGeneric(name, lambdaList.size(), false, sourceSection);
    }

    ISLISPReturnFromNode parseReturnFrom(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 3, 3);
        var name = downcast(args.get(1), Symbol.class);
        var blockId = parserContext.blocks.get(name.identityReference())
                .orElseThrow(() -> new ParsingException(sexpr.sourceSection(), "Bogus return-from"));
        var expression = parseExpressionNode(parserContext, args.get(2));
        return new ISLISPReturnFromNode(blockId, expression, sourceSection);
    }

    ISLISPBlockNode parseBlock(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 2, -1);
        var name = downcast(args.get(1), Symbol.class);
        var newContext = parserContext.pushBlockScope(name.identityReference());
        var blockId = newContext.blocks.get(name.identityReference())
                .orElseThrow(() -> new ParsingException(sexpr.sourceSection(), "Should never happen"));
        ISLISPExpressionNode[] expressions = new ISLISPExpressionNode[args.size() - 2];
        for (int i = 2; i < args.size(); i++) {
            expressions[i - 2] = parseExpressionNode(newContext, args.get(i));
        }
        return new ISLISPBlockNode(blockId, expressions, sourceSection);
    }

    ISLISPLiteralNode parseQuote(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 2, 2);
        return new ISLISPLiteralNode(args.get(1), sourceSection);
    }

    ISLISPExpressionNode parseDebuggerNode(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        return new ISLISPDebuggerNode(sourceSection);
    }

    ISLISPExpressionNode parseIndirectFunCall(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 2, -1);
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        for (Value arg : args.subList(1, args.size())) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        return new ISLISPIndirectFunctionCallNode(
                argNodes.get(0),
                argNodes.subList(1, argNodes.size()).toArray(ISLISPExpressionNode[]::new),
                sourceSection);
    }

    ISLISPExpressionNode parseDirectFunctionCall(
            ParserContext parserContext,
            SourceSection sourceSection,
            Value sexpr
    ) {
        var args = requireList(sexpr, 1, -1);
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        var name = downcast(args.get(0), Symbol.class);
        for (Value arg : args.subList(1, args.size())) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        var maybeVar = parserContext.localFunctions.get(name.identityReference());
        if (maybeVar.isPresent()) {
            var variableContext = maybeVar.get();
            var index = parserContext.frameDepth - variableContext.frameDepth;
            var functionLookup = new ISLISPLexicalIdentifierNode(index, variableContext.slot, name.sourceSection());
            return new ISLISPIndirectFunctionCallNode(
                    functionLookup,
                    argNodes.toArray(ISLISPExpressionNode[]::new),
                    sourceSection);
        } else {
            return new ISLISPGlobalFunctionCallNode(name, argNodes.toArray(ISLISPExpressionNode[]::new), sourceSection);
        }
    }

    ISLISPDirectLambdaCallNode parseDirectLambdaCall(
            ParserContext parserContext,
            SourceSection sourceSection,
            ISLISPLambdaNode lambdaNode,
            Value sexpr
    ) {
        var args = requireList(sexpr, 1, -1);
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        for (Value arg : args.subList(1, args.size())) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        return new ISLISPDirectLambdaCallNode(lambdaNode, argNodes.toArray(ISLISPExpressionNode[]::new), sourceSection);
    }

    ISLISPLambdaNode parseLambda(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 2, -1);
        var restList = args.subList(1, args.size());
        parserContext = parserContext.pushFrameDescriptor();
        var slotsAndNewContext =
                processFrameDescriptorsForFunctionArguments(parserContext.pushClosureScope(), restList.get(0));
        var bodyStatements = restList.stream()
                .skip(1)
                .map(v -> parseExpressionNode(slotsAndNewContext.context, v))
                .toArray(ISLISPExpressionNode[]::new);
        var body = new ISLISPProgn(
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
                sourceSection);
        var rootNode = new ISLISPRootNode(
                ctx.getLanguage(),
                new ISLISPExpressionNode[]{userDefinedFunctionNode},
                parserContext.frameBuilder.build(),
                sourceSection);
        return new ISLISPLambdaNode(rootNode);
    }

    ISLISPDefunNode parseDefun(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 3, -1);
        var restList = args.subList(1, args.size());
        var name = downcast(restList.get(0), Symbol.class);
        parserContext = parserContext.pushFrameDescriptor();
        var slotsAndNewContext = processFrameDescriptorsForFunctionArguments(parserContext, restList.get(1));
        var bodyStatements = restList.stream()
                .skip(2)
                .map(v -> parseExpressionNode(slotsAndNewContext.context, v))
                .toArray(ISLISPExpressionNode[]::new);
        var body = new ISLISPProgn(
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
                sourceSection);
        var rootNode = new ISLISPRootNode(
                ctx.getLanguage(),
                new ISLISPExpressionNode[]{userDefinedFunctionNode},
                parserContext.frameBuilder.build(),
                sourceSection);
        return new ISLISPDefunNode(name, rootNode);
    }

    SlotsAndNewContext processFrameDescriptorsForFunctionArguments(
            ParserContext parserContext,
            Value parameterList
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
                throw new ParsingException(arg.sourceSection(), "Multiple symbols after :rest");
            } else {
                throw new ParsingException(arg.sourceSection(), "Bad parameter list form");
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

    ISLISPProgn parseProgn(ParserContext parserContext, SourceSection sourceSection, Value sexpr, boolean isTopLevel)  {
        var args = requireList(sexpr, 1, -1);
        var bodyStatements = new ArrayList<ISLISPExpressionNode>();
        for (var e: args.subList(1, args.size())) {
            bodyStatements.add(parseExpressionNode(parserContext, e, isTopLevel));
        }
        return new ISLISPProgn(bodyStatements.toArray(ISLISPExpressionNode[]::new), sourceSection);
    }

    ISLISPExpressionNode parseFunctionRef(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 2, 2);
        var name = downcast(args.get(1), Symbol.class);
        var maybeVar = parserContext.localFunctions.get(name.identityReference());
        if (maybeVar.isPresent()) {
            var variableContext = maybeVar.get();
            var index = parserContext.frameDepth - variableContext.frameDepth;
            return new ISLISPLexicalIdentifierNode(index, variableContext.slot, sourceSection);
        } else {
            return new ISLISPFunctionRef(name, sourceSection);
        }
    }

    ISLISPClassRef parseClassRef(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 2, 2);
        var name = downcast(args.get(1), Symbol.class);
        return new ISLISPClassRef(name, sourceSection);
    }

    ISLISPIfNode parseIfNode(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var args = requireList(sexpr, 3, 4);
        var body = new ArrayList<ISLISPExpressionNode>();
        for (var e: args.subList(1, args.size())) {
            body.add(parseExpressionNode(parserContext, e));
        }
        var test = body.get(0);
        var truthy = body.get(1);
        var falsy = body.size() == 2 ? new ISLISPLiteralNode(ISLISPContext.get(null).getNil(), null) : body.get(2);
        return new ISLISPIfNode(test, truthy, falsy, sourceSection);
    }

    ISLISPDefMacro parseDefMacro(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        return new ISLISPDefMacro(parseDefun(parserContext, sourceSection, sexpr));
    }

    ISLISPQuasiquoteNode parseQuasiquote(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
        var qq = QuasiquoteTree.parseQuasiquoteTree(sexpr);
        var childNodes = new ISLISPExpressionNode[qq.expressions().length];
        for (var i = 0; i < childNodes.length; i++) {
            childNodes[i] = parseExpressionNode(parserContext, qq.expressions()[i]);
        }
        return new ISLISPQuasiquoteNode(sourceSection, qq.tree(), childNodes);
    }

    ISLISPLetNode parseLetNode(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
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
                throw new ParsingException(variablesList.get(i).sourceSection(), "Duplicate variable declaration in let");
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
        return new ISLISPLetNode(variableSlots, variableInitializers, body, sourceSection);
    }

    ISLISPLetNode parseLetStarNode(ParserContext parserContext, SourceSection sourceSection, Value sexpr) {
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
        return new ISLISPLetNode(variableSlots, variableInitializers, body, sourceSection);
    }

    ISLISPUnwindProtectNode parseUnwindProtectNode(
            ParserContext parserContext,
            SourceSection sourceSection,
            Value sexpr
    ) {
        var args = requireList(sexpr, 2, -1);
        var body = parseExpressionNode(parserContext, args.get(1));
        var cleanups = new ISLISPExpressionNode[args.size() - 2];
        for (int i = 0; i < cleanups.length; i++) {
            cleanups[i] = parseExpressionNode(parserContext, args.get(i + 2));
        }
        return new ISLISPUnwindProtectNode(body, cleanups, sourceSection);
    }

    <T> T downcast(Value value, Class<T> clazz) throws ParsingException {
        if (clazz.isAssignableFrom(value.getClass())) {
            return (T) value;
        }
        throw new ParsingException(
                value.sourceSection(),
                "Expected " + clazz.getSimpleName() + "; was " + value.getClass().getSimpleName() + ".");
    }

    List<Value> requireList(Value value, int minLength, int maxLength) throws ParsingException {
        if (value instanceof Pair || (value instanceof Symbol s && s.name().equals("nil"))) {
            var lst = Utils.readList(value);
            if (minLength >= 0 && minLength == maxLength && lst.size() != minLength) {
                throw new ParsingException(
                        value.sourceSection(),
                        "Expected a list of length " + minLength + "; was " + lst.size() + ".");
            }
            if (minLength >= 0 && lst.size() < minLength) {
                throw new ParsingException(
                        value.sourceSection(),
                        "Expected a list of at least length " + minLength + "; was " + lst.size() + ".");
            }
            if (maxLength >= 0 && lst.size() > maxLength) {
                throw new ParsingException(
                        value.sourceSection(),
                        "Expected a list of at most length " + minLength + "; was " + lst.size() + ".");
            }
            return lst;
        }
        throw new ParsingException(
                value.sourceSection(),
                "Expected a list; was " + value.getClass().getSimpleName() + ".");
    }

     static class SlotsAndNewContext {
        int[] namedArgsSlots;
        int restArgsSlot;
        ParserContext context;
    }
}
