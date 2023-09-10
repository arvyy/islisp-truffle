package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.nodes.*;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.source.SourceSection;

import java.util.*;
import java.util.stream.Collectors;

public class Parser {
    @CompilerDirectives.TruffleBoundary
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
                span(expressionNodes.get(0).getSourceSection(), expressionNodes.get(expressionNodes.size() - 1).getSourceSection())
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
            var root = new ISLISPRootNode(null, new ISLISPExpressionNode[]{ expression }, null, expression.getSourceSection());
            root.getCallTarget().call();
        }
        if (expression instanceof ISLISPProgn) {
            for (var e: ((ISLISPProgn) expression).getBodyNodes()) {
                executeDefinitions(e);
            }
        }
    }

    Optional<ISLISPExpressionNode> filterMacros(ISLISPExpressionNode expression) {
        if (expression instanceof ISLISPDefMacro)
            return Optional.empty();
        if (expression instanceof ISLISPProgn) {
            var exprs = new ArrayList<ISLISPExpressionNode>();
            for (var e: ((ISLISPProgn) expression).getBodyNodes()) {
                filterMacros(e).ifPresent(exprs::add);
            }
            return Optional.of(new ISLISPProgn(exprs.toArray(ISLISPExpressionNode[]::new), expression.getSourceSection()));
        }
        return Optional.of(expression);
    }

    ISLISPExpressionNode parseExpressionNode(ParserContext parserContext, Value sexpr) {
        return parseExpressionNode(parserContext, sexpr, false);
    }

    ISLISPExpressionNode parseExpressionNode(ParserContext parserContext, Value sexpr, boolean topLevel) {
        if (sexpr instanceof Pair && ((Pair) sexpr).car() instanceof Symbol symbol) {
            var rest = ((Pair) sexpr).cdr();
            var carName = ((Symbol) ((Pair) sexpr).car()).name();
            // builtins
            switch (carName) {
                case "defconstant":
                    if (!topLevel)
                        throw new RuntimeException();
                    return parseDefConstant(parserContext, sexpr.sourceSection(), rest);
                case "defdynamic":
                    if (!topLevel)
                        throw new RuntimeException();
                    return parseDefDynamic(parserContext, sexpr.sourceSection(), rest);
                case "defgeneric":
                    if (!topLevel)
                        throw new RuntimeException();
                    return parseDefGeneric(parserContext, sexpr.sourceSection(), rest);
                case "defglobal":
                    if (!topLevel)
                        throw new RuntimeException();
                    return parseDefGlobal(parserContext, sexpr.sourceSection(), rest);
                case "defmacro":
                    if (!topLevel)
                        throw new RuntimeException();
                    return parseDefMacro(parserContext, sexpr.sourceSection(), rest);
                case "defun":
                    if (!topLevel)
                        throw new RuntimeException();
                    return parseDefun(parserContext, sexpr.sourceSection(), rest);
                case "defmethod":
                    if (!topLevel)
                        throw new RuntimeException();
                    return parseDefMethod(parserContext, sexpr.sourceSection(), rest);
                case "progn":
                    return parseProgn(parserContext, sexpr.sourceSection(), rest, topLevel);
                case "funcall":
                    return parseIndirectFunCall(parserContext, sexpr.sourceSection(), rest);
                case "function":
                    return parseFunctionRef(parserContext, sexpr.sourceSection(), rest);
                case "lambda":
                    return parseLambda(parserContext, sexpr.sourceSection(), rest);
                case "if":
                    return parseIfNode(parserContext, sexpr.sourceSection(), rest);
                case "debugger":
                    return parseDebuggerNode(parserContext, sexpr.sourceSection(), rest);
                case "quote":
                    return parseQuote(parserContext, sexpr.sourceSection(), rest);
                case "quasiquote":
                    return parseQuasiquote(parserContext, sexpr.sourceSection(), sexpr);
                case "class":
                    return parseClassRef(parserContext, sexpr.sourceSection(), rest);
                case "block":
                    return parseBlock(parserContext, sexpr.sourceSection(), rest);
                case "return-from":
                    return parseReturnFrom(parserContext, sexpr.sourceSection(), rest);
                case "let":
                    return parseLetNode(parserContext, sexpr.sourceSection(), rest);
                case "let*":
                    return parseLetStarNode(parserContext, sexpr.sourceSection(), rest);
                case "dynamic":
                    return parseDynamic(parserContext, sexpr.sourceSection(), rest);
                case "dynamic-let":
                    return parseDynamicLet(parserContext, sexpr.sourceSection(), rest);
                case "set-dynamic":
                    return parseSetDynamic(parserContext, sexpr.sourceSection(), rest);
                case "setq":
                    return parseSetq(parserContext, sexpr.sourceSection(), rest);
                case "tagbody":
                    return parseTagBody(parserContext, sexpr.sourceSection(), rest);
                case "go":
                    return parseTagBodyGo(parserContext, sexpr.sourceSection(), rest);
                case "unwind-protect":
                    return parseUnwindProtectNode(parserContext, sexpr.sourceSection(), rest);
            }
            // macros
            if ("setf".equals(carName)) {
                var args = Utils.readList(rest);
                var form = macroExpand(args.get(0), false);
                var value = args.get(1);
                if (form instanceof Symbol s) {
                    return parseExpressionNode(parserContext, Utils.listToValue(List.of(
                            ISLISPContext.get(null).namedSymbol("setq"),
                            s,
                            value
                    ), sexpr.sourceSection()));
                }
                var formList = Utils.readList(form);
                var setfDispatchSymbol = (Symbol) formList.get(0);
                var setfDispatch = ISLISPContext.get(null).lookupSetfTransformer(setfDispatchSymbol.identityReference());
                if (setfDispatch == null) throw new RuntimeException("Unknown setf form");
                return parseExpressionNode(parserContext, setfDispatch.transform(formList, value, sexpr.sourceSection()));
            }
            var expanded = macroExpand(sexpr, true);
            if (sexpr == expanded) {
                return parseDirectFunctionCall(parserContext, sexpr.sourceSection(), symbol, ((Pair) sexpr).cdr());
            } else {
                return parseExpressionNode(parserContext, expanded, topLevel);
            }
            /*
            var symbol = ISLISPContext.get(null).namedSymbol(carName);
            var maybeMacro = ISLISPContext.get(null).lookupMacro(symbol.identityReference());
            if (maybeMacro != null) {
                var args = new ArrayList<Value>();
                args.add(null); // closure param
                Iterable<Value> it = rest.equals(ISLISPContext.get(null).getNIL())? List.of() : (Pair) rest;
                for (var e: it) {
                    args.add(e);
                }
                var transformedSexpr = (Value) maybeMacro.callTarget().call(args.toArray());
                return parseExpressionNode(parserContext, transformedSexpr, topLevel);
            }
            return parseDirectFunctionCall(parserContext, sexpr.sourceSection(), symbol, ((Pair) sexpr).cdr());
             */
        }
        //a list pattern, but car isn't a symbol: it might be immediate lambda call
        if (sexpr instanceof Pair) {
            var car = ((Pair) sexpr).car();
            if (car instanceof Pair firstPos) {
                if (firstPos.car() instanceof Symbol maybeLambda) {
                    if (maybeLambda.name().equals("lambda")) {
                        var lambda = parseLambda(parserContext, firstPos.sourceSection(), firstPos.cdr());
                        return parseDirectLambdaCall(parserContext, sexpr.sourceSection(), lambda, ((Pair) sexpr).cdr());
                    }
                }
            }
        }
        if (sexpr instanceof LispInteger || sexpr instanceof LispChar) {
            return new ISLISPLiteralNode(sexpr, sexpr.sourceSection());
        }
        if (sexpr instanceof Symbol) {
            var symbol = (Symbol) sexpr;
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
        //TODO
        throw new RuntimeException();
    }

    private ISLISPDefGlobalNode parseDefGlobal(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        var name = (Symbol) args.get(0);
        var init = parseExpressionNode(parserContext, args.get(1));
        return new ISLISPDefGlobalNode(name, init, sourceSection);
    }

    private ISLISPDefConstantNode parseDefConstant(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        var name = (Symbol) args.get(0);
        var init = parseExpressionNode(parserContext, args.get(1));
        return new ISLISPDefConstantNode(name, init, sourceSection);
    }

    private ISLISPTagBodyGoNode parseTagBodyGo(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        var tagSymbol = (Symbol) args.get(0);
        var maybeTagId = parserContext.tagbodyTags.get(tagSymbol.identityReference());
        if (maybeTagId.isPresent()) {
            return new ISLISPTagBodyGoNode(maybeTagId.get(), sourceSection);
        }
        throw new RuntimeException("Not found tag " + tagSymbol);
    }

    private ISLISPTagBodyNode parseTagBody(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        var expressions = new ArrayList<Value>();
        var tags = new HashMap<SymbolReference, Integer>();
        var tagSymbols = new ArrayList<SymbolReference>();
        for (var arg: args) {
            if (arg instanceof Symbol tag) {
                tags.put(tag.identityReference(), expressions.size());
                tagSymbols.add(tag.identityReference());
            } else if (arg instanceof Pair) {
                expressions.add(arg);
            } else {
                throw new RuntimeException();
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

    private ISLISPSetqNode parseSetq(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        var name = (Symbol) args.get(0);
        var expr = parseExpressionNode(parserContext, args.get(1));
        var maybeVar = parserContext.variables.get(name.identityReference());
        if (maybeVar.isPresent()) {
            var variableContext = maybeVar.get();
            var index = parserContext.frameDepth - variableContext.frameDepth;
            return new ISLISPSetqNode(index, variableContext.slot, expr, sourceSection);
        } else {
            return new ISLISPSetqNode(name, expr, sourceSection);
        }
    }

    static Value macroExpand(Value form, boolean single) {
        if (form instanceof Pair p && p.car() instanceof Symbol symbol) {
            var rest = p.cdr();
            var maybeMacro = ISLISPContext.get(null).lookupMacro(symbol.identityReference());
            if (maybeMacro != null) {
                var args = new ArrayList<Value>();
                args.add(null); // closure param
                Iterable<Value> it = rest.equals(ISLISPContext.get(null).getNIL())? List.of() : (Pair) rest;
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

    private ISLISPSetDynamicNode parseSetDynamic(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        var initalizer = parseExpressionNode(parserContext, args.get(0));
        var symbol = (Symbol) args.get(1);
        return new ISLISPSetDynamicNode(symbol, initalizer, sourceSection);
    }

    private ISLISPDynamicLetNode parseDynamicLet(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        var bindingList = Utils.readList(args.get(0));
        var symbols = new Symbol[bindingList.size()];
        var initializers = new ISLISPExpressionNode[bindingList.size()];
        for (int i = 0; i < symbols.length; i++) {
            var bindingEntry = Utils.readList(bindingList.get(i));
            symbols[i] = (Symbol) bindingEntry.get(0);
            initializers[i] = parseExpressionNode(parserContext, bindingEntry.get(1));
        }
        var body = new ISLISPExpressionNode[args.size() - 1];
        for (int i = 0; i < body.length; i++) {
            body[i] = parseExpressionNode(parserContext, args.get(i + 1));
        }
        return new ISLISPDynamicLetNode(symbols, initializers, body, sourceSection);
    }

    private static ISLISPDynamicLookupNode parseDynamic(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        return new ISLISPDynamicLookupNode((Symbol) args.get(0), sourceSection);
    }

    private ISLISPDefDynamicNode parseDefDynamic(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        return new ISLISPDefDynamicNode((Symbol) args.get(0), parseExpressionNode(parserContext, args.get(1)), sourceSection);
    }

    private ISLISPDefMethodNode parseDefMethod(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        var name = (Symbol) args.get(0);
        var methodQualifiers = new ArrayList<String>();
        for (int i = 1; i < args.size(); i++) {
            if (args.get(i) instanceof Symbol s && !s.name().equals("nil")) {
                methodQualifiers.add(s.name());
            } else {
                break;
            }
        }
        var paramListIndex = 1 + methodQualifiers.size();
        var parameterList = Utils.readList(args.get(paramListIndex));
        var plainParamList = new ArrayList<Symbol>();
        var paramTypes = new ArrayList<Symbol>();
        for (var el: parameterList) {
            if (el instanceof Symbol s) {
                plainParamList.add(s);
                paramTypes.add(ISLISPContext.get(null).namedSymbol("<object>"));
            } else if (el instanceof Pair p) {
                var paramWithType = Utils.readList(p);
                if (paramWithType.size() != 2) {
                    throw new RuntimeException();
                }
                if (paramWithType.get(0) instanceof Symbol paramName) {
                    plainParamList.add(paramName);
                } else {
                    throw new RuntimeException();
                }
                if (paramWithType.get(1) instanceof Symbol paramClass) {
                    paramTypes.add(paramClass);
                } else {
                    throw new RuntimeException();
                }
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
                throw new RuntimeException("Incompatible method qualifiers");
            }
            methodQualifier = ISLISPDefMethodNode.MethodQualifier.before;
        }
        if (methodQualifiers.contains(":after")) {
            if (methodQualifier != ISLISPDefMethodNode.MethodQualifier.none) {
                throw new RuntimeException("Incompatible method qualifiers");
            }
            methodQualifier = ISLISPDefMethodNode.MethodQualifier.after;
        }
        if (methodQualifiers.contains(":around")) {
            if (methodQualifier != ISLISPDefMethodNode.MethodQualifier.none) {
                throw new RuntimeException("Incompatible method qualifiers");
            }
            methodQualifier = ISLISPDefMethodNode.MethodQualifier.around;
        }
        if (methodQualifier == ISLISPDefMethodNode.MethodQualifier.before || methodQualifier == ISLISPDefMethodNode.MethodQualifier.after) {
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
        var body = new ISLISPProgn(bodyStatements, null);
        return new ISLISPDefMethodNode(
                methodQualifier,
                name,
                paramTypes.toArray(Symbol[]::new),
                parserContext.frameBuilder.build(),
                slotsAndNewContext.namedArgsSlots,
                slotsAndNewContext.restArgsSlot,
                callNextMethodSlot,
                hasNextMethodSlot,
                body,
                sourceSection);
    }

    private static ISLISPDefGeneric parseDefGeneric(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        var name = (Symbol) args.get(0);
        var lambdaList = Utils.readList(args.get(1));
        return new ISLISPDefGeneric(name, lambdaList.size(), false, sourceSection);
    }

    private ISLISPReturnFromNode parseReturnFrom(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        var name = (Symbol) args.get(0);
        var blockId = parserContext.blocks.get(name.identityReference())
                .orElseThrow(() -> new RuntimeException("Bogus return-from"));
        var expression = parseExpressionNode(parserContext, args.get(1));
        return new ISLISPReturnFromNode(blockId, expression, sourceSection);
    }

    private ISLISPBlockNode parseBlock(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        var name = (Symbol) args.get(0);
        var newContext = parserContext.pushBlockScope(name.identityReference());
        var blockId = newContext.blocks.get(name.identityReference()).get();
        ISLISPExpressionNode[] expressions = new ISLISPExpressionNode[args.size() - 1];
        for (int i = 1; i < args.size(); i++) {
            expressions[i - 1] = parseExpressionNode(newContext, args.get(i));
        }
        return new ISLISPBlockNode(blockId, expressions, sourceSection);
    }

    private ISLISPLiteralNode parseQuote(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var car = ((Pair) rest).car();
        return new ISLISPLiteralNode(car, sourceSection);
    }

    private ISLISPExpressionNode parseDebuggerNode(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        return new ISLISPDebuggerNode(sourceSection);
    }

    ISLISPExpressionNode parseIndirectFunCall(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = Utils.readList(rest);
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        for (Value arg : args) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        if (argNodes.isEmpty()) {
            throw new RuntimeException();
        }
        return new ISLISPIndirectFunctionCallNode(argNodes.get(0), argNodes.subList(1, argNodes.size()).toArray(ISLISPExpressionNode[]::new), sourceSection);
    }

    ISLISPExpressionNode parseDirectFunctionCall(ParserContext parserContext, SourceSection sourceSection, Symbol name, Value rest) {
        Iterable<Value> args = rest instanceof Pair? (Pair) rest : List.of();
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        for (Value arg : args) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        var maybeVar = parserContext.localFunctions.get(name.identityReference());
        if (maybeVar.isPresent()) {
            var variableContext = maybeVar.get();
            var index = parserContext.frameDepth - variableContext.frameDepth;
            var functionLookup = new ISLISPLexicalIdentifierNode(index, variableContext.slot, name.sourceSection());
            return new ISLISPIndirectFunctionCallNode(functionLookup, argNodes.toArray(ISLISPExpressionNode[]::new), sourceSection);
        } else {
            return new ISLISPGlobalFunctionCallNode(name, argNodes.toArray(ISLISPExpressionNode[]::new), sourceSection);
        }
    }

    ISLISPDirectLambdaCallNode parseDirectLambdaCall(ParserContext parserContext, SourceSection sourceSection, ISLISPLambdaNode lambdaNode, Value rest) {
        Iterable<Value> args = rest instanceof Pair? (Pair) rest : List.of();
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        for (Value arg : args) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        return new ISLISPDirectLambdaCallNode(lambdaNode, argNodes.toArray(ISLISPExpressionNode[]::new), sourceSection);
    }

    ISLISPLambdaNode parseLambda(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var restList = Utils.readList(rest);
        parserContext = parserContext.pushFrameDescriptor();
        var slotsAndNewContext = processFrameDescriptorsForFunctionArguments(parserContext.pushClosureScope(), restList.get(0));
        var bodyStatements = restList.stream()
                .skip(1)
                .map(v -> parseExpressionNode(slotsAndNewContext.context, v))
                .toArray(ISLISPExpressionNode[]::new);
        var body = new ISLISPProgn(bodyStatements, null);
        return new ISLISPLambdaNode(parserContext.frameBuilder.build(), slotsAndNewContext.namedArgsSlots, slotsAndNewContext.restArgsSlot, body, sourceSection);
    }

    ISLISPDefunNode parseDefun(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var restList = Utils.readList(rest);
        var name = ((Symbol) restList.get(0)).name();
        parserContext = parserContext.pushFrameDescriptor();
        var slotsAndNewContext = processFrameDescriptorsForFunctionArguments(parserContext, restList.get(1));
        var bodyStatements = restList.stream()
                .skip(2)
                .map(v -> parseExpressionNode(slotsAndNewContext.context, v))
                .toArray(ISLISPExpressionNode[]::new);
        var body = new ISLISPProgn(bodyStatements, null);
        var symbol = ISLISPContext.get(null).namedSymbol(name);
        return new ISLISPDefunNode(symbol, parserContext.frameBuilder.build(), slotsAndNewContext.namedArgsSlots, slotsAndNewContext.restArgsSlot, body, sourceSection);
    }

    SlotsAndNewContext processFrameDescriptorsForFunctionArguments(
            ParserContext parserContext,
            Value parameterList
    ) {
        var args = Utils.readList(parameterList)
                .stream()
                .peek(v -> {
                    if (!(v instanceof Symbol)) {
                        throw new RuntimeException();
                    }
                })
                .map(v -> (Symbol) v)
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
        int NAMED_ARGS = 0;
        int AFTER_REST_KW = 1;
        int AFTER_REST_ARG = 2;
        int state = NAMED_ARGS;
        for (var arg: parameterList) {
            var isRestKw = "&rest".equals(arg.name()) || ":rest".equals(arg.name());
            if (state == NAMED_ARGS && !isRestKw) {
                var slot = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
                positionalArgumentSlots.add(slot);
                var variableContext = new ParserContext.VariableContext();
                variableContext.frameDepth = parserContext.frameDepth;
                variableContext.slot = slot;
                variables.put(arg.identityReference(), variableContext);
            } else if (state == NAMED_ARGS && isRestKw) {
                state = AFTER_REST_KW;
            } else if (state == AFTER_REST_KW && !isRestKw) {
                state = AFTER_REST_ARG;
                restSlot = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
                var variableContext = new ParserContext.VariableContext();
                variableContext.frameDepth = parserContext.frameDepth;
                variableContext.slot = restSlot;
                variables.put(arg.identityReference(), variableContext);
            } else if (state == AFTER_REST_ARG) {
                throw new RuntimeException("Multiple symbols after :rest");
            } else {
                throw new RuntimeException("Bad parameter list form");
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

    ISLISPProgn parseProgn(ParserContext parserContext, SourceSection sourceSection, Value body, boolean isTopLevel)  {
        var bodyStatements = new ArrayList<ISLISPExpressionNode>();
        for (var e: (Pair) body) {
            bodyStatements.add(parseExpressionNode(parserContext, e, isTopLevel));
        }
        return new ISLISPProgn(bodyStatements.toArray(ISLISPExpressionNode[]::new), sourceSection);
    }

    ISLISPExpressionNode parseFunctionRef(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var pair = (Pair) rest;
        var name = (Symbol) pair.car();
        var maybeVar = parserContext.localFunctions.get(name.identityReference());
        if (maybeVar.isPresent()) {
            var variableContext = maybeVar.get();
            var index = parserContext.frameDepth - variableContext.frameDepth;
            return new ISLISPLexicalIdentifierNode(index, variableContext.slot, sourceSection);
        } else {
            return new ISLISPFunctionRef(name, sourceSection);
        }
    }

    ISLISPClassRef parseClassRef(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var pair = (Pair) rest;
        var name = ((Symbol) pair.car()).name();
        var symbol = ISLISPContext.get(null).namedSymbol(name);
        return new ISLISPClassRef(symbol, sourceSection);
    }

    ISLISPIfNode parseIfNode(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var body = new ArrayList<ISLISPExpressionNode>();
        for (var e: (Pair) rest) {
            body.add(parseExpressionNode(parserContext, e));
        }
        var test = body.get(0);
        var truthy = body.get(1);
        var falsy = body.size() == 2? new ISLISPLiteralNode(ISLISPContext.get(null).getNIL(), null) : body.get(2);
        return new ISLISPIfNode(test, truthy, falsy, sourceSection);
    }

    ISLISPDefMacro parseDefMacro(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        return new ISLISPDefMacro(parseDefun(parserContext, sourceSection, rest));
    }

    ISLISPQuasiquoteNode parseQuasiquote(ParserContext parserContext, SourceSection sourceSection, Value me) {
        var qq = QuasiquoteTree.parseQuasiquoteTree(me);
        var childNodes = new ISLISPExpressionNode[qq.expressions().length];
        for (var i = 0; i < childNodes.length; i++) {
            childNodes[i] = parseExpressionNode(parserContext, qq.expressions()[i]);
        }
        return new ISLISPQuasiquoteNode(sourceSection, qq.tree(), childNodes);
    }

    ISLISPLetNode parseLetNode(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var lst = Utils.readList(rest);
        var variablesList = Utils.readList(lst.get(0));
        var bodyExpressions = lst.subList(1, lst.size());
        var variableSlots = new int[variablesList.size()];
        var variableInitializers = new ISLISPExpressionNode[variablesList.size()];
        var variableNameMap = new HashMap<SymbolReference, ParserContext.VariableContext>();
        for (var i = 0; i < variablesList.size(); i++) {
            var variable = Utils.readList(variablesList.get(i));
            var variableName = (Symbol) variable.get(0);
            var variableInitializer = variable.get(1);
            variableSlots[i] = parserContext.frameBuilder.addSlot(FrameSlotKind.Object, null, null);
            variableInitializers[i] = parseExpressionNode(parserContext, variableInitializer);
            if (variableNameMap.containsKey(variableName.identityReference())) {
                throw new RuntimeException();
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

    ISLISPLetNode parseLetStarNode(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var lst = Utils.readList(rest);
        var variablesList = Utils.readList(lst.get(0));
        var bodyExpressions = lst.subList(1, lst.size());
        var variableSlots = new int[variablesList.size()];
        var variableInitializers = new ISLISPExpressionNode[variablesList.size()];
        for (var i = 0; i < variablesList.size(); i++) {
            var variable = Utils.readList(variablesList.get(i));
            var variableName = (Symbol) variable.get(0);
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

    ISLISPUnwindProtectNode parseUnwindProtectNode(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var lst = Utils.readList(rest);
        var body = parseExpressionNode(parserContext, lst.get(0));
        var cleanups = new ISLISPExpressionNode[lst.size() - 1];
        for (int i = 0; i < cleanups.length; i++) {
            cleanups[i] = parseExpressionNode(parserContext, lst.get(i + 1));
        }
        return new ISLISPUnwindProtectNode(body, cleanups, sourceSection);
    }


    private static class SlotsAndNewContext {
        int[] namedArgsSlots;
        int restArgsSlot;
        ParserContext context;
    }
}