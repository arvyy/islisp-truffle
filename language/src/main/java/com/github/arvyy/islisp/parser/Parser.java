package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.github.arvyy.islisp.nodes.*;
import com.github.arvyy.islisp.runtime.LispInteger;
import com.github.arvyy.islisp.runtime.Pair;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Parser {
    public ISLISPRootNode parseRootNode(ISLISPTruffleLanguage language, List<Value> content) {
        var expressionNodes = new ArrayList<ISLISPExpressionNode>();
        var parserContext = new ParserContext(0, new LexicalScope<>());
        for (var v: content) {
            var expression = parseExpressionNode(parserContext, v, true);
            executeDefinitions(expression); // execute definitions to be available for macro procedure runs
            filterMacros(expression);
            expressionNodes.add(expression);
        }
        var ctx = ISLISPContext.get(null);
        ctx.reset();
        var root = new ISLISPRootNode(
                language,
                expressionNodes.toArray(ISLISPExpressionNode[]::new));
        return root;
    }

    void executeDefinitions(ISLISPExpressionNode expression) {
        if (expression.isDefinitionNode()) {
            var root = new ISLISPRootNode(null, new ISLISPExpressionNode[]{ expression });
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
            return Optional.of(new ISLISPProgn(exprs.toArray(ISLISPExpressionNode[]::new)));
        }
        return Optional.of(expression);
    }

    ISLISPExpressionNode parseExpressionNode(ParserContext parserContext, Value sexpr) {
        return parseExpressionNode(parserContext, sexpr, false);
    }

    ISLISPExpressionNode parseExpressionNode(ParserContext parserContext, Value sexpr, boolean topLevel) {
        if (sexpr instanceof Pair && ((Pair) sexpr).car() instanceof Symbol) {
            var rest = ((Pair) sexpr).cdr();
            var carName = ((Symbol) ((Pair) sexpr).car()).name();
            // builtins
            switch (carName) {
                case "defmacro":
                    if (!topLevel)
                        throw new RuntimeException();
                    return parseDefMacro(parserContext, rest);
                case "defun":
                    if (!topLevel)
                        throw new RuntimeException();
                    return parseDefun(parserContext, rest);
                case "progn":
                    return parseProgn(parserContext, rest, topLevel);
                case "funcall":
                    return parseIndirectFunCall(parserContext, rest);
                case "function":
                    return parseFunctionRef(rest);
                case "lambda":
                    return parseLambda(parserContext, rest);
                case "if":
                    return parseIfNode(parserContext, rest);
            }
            // macros
            var maybeMacro = ISLISPContext.get(null).lookupMacro(carName);
            if (maybeMacro != null) {
                var args = new ArrayList<Value>();
                args.add(null); // closure param
                Iterable<Value> it = rest.equals(Symbol.NIL)? List.of() : (Pair) rest;
                for (var e: it) {
                    args.add(e);
                }
                var transformedSexpr = (Value) maybeMacro.callTarget().call(args.toArray());
                return parseExpressionNode(parserContext, transformedSexpr, topLevel);
            }
            return parseDirectFunctionCall(parserContext, ((Symbol) ((Pair) sexpr).car()).name(), ((Pair) sexpr).cdr());
        }
        if (sexpr instanceof LispInteger) {
            return new ISLISPLiteralNode(sexpr);
        }
        if (sexpr instanceof Symbol) {
            var maybeLexicalSlot = parserContext.variables.get(((Symbol) sexpr).name());
            if (maybeLexicalSlot.isPresent()) {
                var variableContext = maybeLexicalSlot.get();
                var index = parserContext.frameDepth - variableContext.frameDepth;
                var slot = variableContext.slot;
                return new ISLISPLexicalIdentifierNode(index, slot);
            } else {
                //TODO global lookup node
            }
        }
        //TODO
        throw new RuntimeException();
    }

    ISLISPExpressionNode parseIndirectFunCall(ParserContext parserContext, Value rest) {
        Iterable<Value> args = rest instanceof Pair? (Pair) rest : List.of();
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        for (Value arg : args) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        if (argNodes.isEmpty()) {
            throw new RuntimeException();
        }
        return new ISLISPIndirectFunctionCallNode(argNodes.toArray(ISLISPExpressionNode[]::new));
    }

    ISLISPExpressionNode parseDirectFunctionCall(ParserContext parserContext, String carName, Value rest) {
        Iterable<Value> args = rest instanceof Pair? (Pair) rest : List.of();
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        for (Value arg : args) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        return new ISLISPDirectFunctionCallNode(carName, argNodes.toArray(ISLISPExpressionNode[]::new));
    }

    ISLISPLambdaNode parseLambda(ParserContext parserContext, Value rest) {
        var restList = new ArrayList<Value>();
        for (var e: (Pair) rest) {
            restList.add(e);
        }
        var newParserContext = parserContext.pushClosureScope();
        var positionalArgumentSlots = new ArrayList<Integer>();
        Iterable<Value> args = restList.get(0).equals(Symbol.NIL)? List.of() : (Pair) restList.get(1);
        var frameDescriptorBuilder = FrameDescriptor.newBuilder();
        //TODO factor out to reduce repeat with parseDefun
        for (var v: args) {
            var arg = (Symbol) v;
            var slot = frameDescriptorBuilder.addSlot(FrameSlotKind.Object, null, null);
            positionalArgumentSlots.add(slot);
            var variableContext = new VariableContext();
            variableContext.frameDepth = newParserContext.frameDepth;
            variableContext.slot = slot;
            newParserContext.variables.put(arg.name(), variableContext);
        }
        var argSlots = new int[positionalArgumentSlots.size()];
        for (var i = 0; i < positionalArgumentSlots.size(); i++) {
            argSlots[i] = positionalArgumentSlots.get(i);
        }
        var bodyStatements = restList.stream()
                .skip(1)
                .map(v -> parseExpressionNode(newParserContext, v))
                .toArray(ISLISPExpressionNode[]::new);
        var body = new ISLISPProgn(bodyStatements);
        return new ISLISPLambdaNode(frameDescriptorBuilder.build(), argSlots, body);
    }

    ISLISPDefunNode parseDefun(ParserContext parserContext, Value rest) {
        var restList = new ArrayList<Value>();
        for (var e: (Pair) rest) {
            restList.add(e);
        }
        var newParserContext = parserContext.pushClosureScope();
        var name = ((Symbol) restList.get(0)).name();
        var positionalArgumentSlots = new ArrayList<Integer>();
        Iterable<Value> args = restList.get(1).equals(Symbol.NIL)? List.of() : (Pair) restList.get(1);
        var frameDescriptorBuilder = FrameDescriptor.newBuilder();
        for (var v: args) {
            var arg = (Symbol) v;
            var slot = frameDescriptorBuilder.addSlot(FrameSlotKind.Object, null, null);
            positionalArgumentSlots.add(slot);
            var variableContext = new VariableContext();
            variableContext.frameDepth = newParserContext.frameDepth;
            variableContext.slot = slot;
            newParserContext.variables.put(arg.name(), variableContext);
        }
        var argSlots = new int[positionalArgumentSlots.size()];
        for (var i = 0; i < positionalArgumentSlots.size(); i++) {
            argSlots[i] = positionalArgumentSlots.get(i);
        }
        var bodyStatements = restList.stream()
                .skip(2)
                .map(v -> parseExpressionNode(newParserContext, v))
                .toArray(ISLISPExpressionNode[]::new);
        var body = new ISLISPProgn(bodyStatements);
        return new ISLISPDefunNode(name, frameDescriptorBuilder.build(), argSlots, body);
    }

    ISLISPProgn parseProgn(ParserContext parserContext, Value body, boolean isTopLevel)  {
        var bodyStatements = new ArrayList<ISLISPExpressionNode>();
        for (var e: (Pair) body) {
            bodyStatements.add(parseExpressionNode(parserContext, e, isTopLevel));
        }
        return new ISLISPProgn(bodyStatements.toArray(ISLISPExpressionNode[]::new));
    }

    ISLISPFunctionRef parseFunctionRef(Value rest) {
        var pair = (Pair) rest;
        var name = ((Symbol) pair.car()).name();
        return new ISLISPFunctionRef(name);
    }

    ISLISPIfNode parseIfNode(ParserContext parserContext, Value rest) {
        var body = new ArrayList<ISLISPExpressionNode>();
        for (var e: (Pair) rest) {
            body.add(parseExpressionNode(parserContext, e));
        }
        var test = body.get(0);
        var truthy = body.get(1);
        var falsy = body.size() == 2? new ISLISPLiteralNode(Symbol.NIL) : body.get(2);
        return new ISLISPIfNode(test, truthy, falsy);
    }

    ISLISPDefMacro parseDefMacro(ParserContext parserContext, Value rest) {
        return new ISLISPDefMacro(parseDefun(parserContext, rest));
    }

}

class VariableContext {
    int frameDepth;
    int slot;
}

class ParserContext {
    final int frameDepth;
    final LexicalScope<String, VariableContext> variables;

    ParserContext(int frameDepth, LexicalScope<String, VariableContext> variables) {
        this.frameDepth = frameDepth;
        this.variables = variables;
    }

    ParserContext pushClosureScope() {
        return new ParserContext(frameDepth + 1, new LexicalScope<>(variables));
    }

}