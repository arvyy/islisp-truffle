package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.github.arvyy.islisp.nodes.*;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.source.SourceSection;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Parser {
    @CompilerDirectives.TruffleBoundary
    public ISLISPRootNode parseRootNode(ISLISPTruffleLanguage language, List<Value> content) {
        var expressionNodes = new ArrayList<ISLISPExpressionNode>();
        var parserContext = new ParserContext(0, new LexicalScope<>(), new IdGen(), new LexicalScope<>());
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
            var root = new ISLISPRootNode(null, new ISLISPExpressionNode[]{ expression }, expression.getSourceSection());
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
        if (sexpr instanceof Pair && ((Pair) sexpr).car() instanceof Symbol) {
            var rest = ((Pair) sexpr).cdr();
            var carName = ((Symbol) ((Pair) sexpr).car()).name();
            // builtins
            switch (carName) {
                case "defmacro":
                    if (!topLevel)
                        throw new RuntimeException();
                    return parseDefMacro(parserContext, sexpr.sourceSection(), rest);
                case "defun":
                    if (!topLevel)
                        throw new RuntimeException();
                    return parseDefun(parserContext, sexpr.sourceSection(), rest);
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
            }
            // macros
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
        if (sexpr instanceof LispInteger) {
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
                //TODO global lookup node
            }
        }
        //TODO
        throw new RuntimeException();
    }

    private ISLISPReturnFromNode parseReturnFrom(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = new ArrayList<Value>();
        for (var v: (Pair) rest) {
            args.add(v);
        }
        var name = (Symbol) args.get(0);
        var blockId = parserContext.blocks.get(name.identityReference())
                .orElseThrow(() -> new RuntimeException("Bogus return-from"));
        var expression = parseExpressionNode(parserContext, args.get(1));
        return new ISLISPReturnFromNode(blockId, expression, sourceSection);
    }

    private ISLISPBlockNode parseBlock(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var args = new ArrayList<Value>();
        for (var v: (Pair) rest) {
            args.add(v);
        }
        var name = (Symbol) args.get(0);
        var newContext = parserContext.pushBlockScope(name.identityReference());
        var blockId = newContext.blocks.get(name.identityReference()).get();
        ISLISPExpressionNode expressions[] = new ISLISPExpressionNode[args.size() - 1];
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
        Iterable<Value> args = rest instanceof Pair? (Pair) rest : List.of();
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        for (Value arg : args) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        if (argNodes.isEmpty()) {
            throw new RuntimeException();
        }
        return new ISLISPIndirectFunctionCallNode(argNodes.toArray(ISLISPExpressionNode[]::new), sourceSection);
    }

    ISLISPExpressionNode parseDirectFunctionCall(ParserContext parserContext, SourceSection sourceSection, Symbol name, Value rest) {
        Iterable<Value> args = rest instanceof Pair? (Pair) rest : List.of();
        var argNodes = new ArrayList<ISLISPExpressionNode>();
        for (Value arg : args) {
            argNodes.add(parseExpressionNode(parserContext, arg));
        }
        return new ISLISPDirectFunctionCallNode(name, argNodes.toArray(ISLISPExpressionNode[]::new), sourceSection);
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
        var restList = new ArrayList<Value>();
        for (var e: (Pair) rest) {
            restList.add(e);
        }
        var newParserContext = parserContext.pushClosureScope();
        var positionalArgumentSlots = new ArrayList<Integer>();
        Iterable<Value> args = restList.get(0).equals(ISLISPContext.get(null).getNIL())? List.of() : (Pair) restList.get(0);
        var frameDescriptorBuilder = FrameDescriptor.newBuilder();
        //TODO factor out to reduce repeat with parseDefun
        for (var v: args) {
            var arg = (Symbol) v;
            var slot = frameDescriptorBuilder.addSlot(FrameSlotKind.Object, null, null);
            positionalArgumentSlots.add(slot);
            var variableContext = new VariableContext();
            variableContext.frameDepth = newParserContext.frameDepth;
            variableContext.slot = slot;
            newParserContext.variables.put(arg.identityReference(), variableContext);
        }
        var argSlots = new int[positionalArgumentSlots.size()];
        for (var i = 0; i < positionalArgumentSlots.size(); i++) {
            argSlots[i] = positionalArgumentSlots.get(i);
        }
        var bodyStatements = restList.stream()
                .skip(1)
                .map(v -> parseExpressionNode(newParserContext, v))
                .toArray(ISLISPExpressionNode[]::new);
        var body = new ISLISPProgn(bodyStatements, null);
        return new ISLISPLambdaNode(frameDescriptorBuilder.build(), argSlots, body, sourceSection);
    }

    ISLISPDefunNode parseDefun(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var restList = new ArrayList<Value>();
        for (var e: (Pair) rest) {
            restList.add(e);
        }
        var newParserContext = parserContext.pushClosureScope();
        var name = ((Symbol) restList.get(0)).name();
        var positionalArgumentSlots = new ArrayList<Integer>();
        Iterable<Value> args = restList.get(1).equals(ISLISPContext.get(null).getNIL())? List.of() : (Pair) restList.get(1);
        var frameDescriptorBuilder = FrameDescriptor.newBuilder();
        for (var v: args) {
            var arg = (Symbol) v;
            var slot = frameDescriptorBuilder.addSlot(FrameSlotKind.Object, null, null);
            positionalArgumentSlots.add(slot);
            var variableContext = new VariableContext();
            variableContext.frameDepth = newParserContext.frameDepth;
            variableContext.slot = slot;
            newParserContext.variables.put(arg.identityReference(), variableContext);
        }
        var argSlots = new int[positionalArgumentSlots.size()];
        for (var i = 0; i < positionalArgumentSlots.size(); i++) {
            argSlots[i] = positionalArgumentSlots.get(i);
        }
        var bodyStatements = restList.stream()
                .skip(2)
                .map(v -> parseExpressionNode(newParserContext, v))
                .toArray(ISLISPExpressionNode[]::new);
        var body = new ISLISPProgn(bodyStatements, null);
        var symbol = ISLISPContext.get(null).namedSymbol(name);
        return new ISLISPDefunNode(symbol, frameDescriptorBuilder.build(), argSlots, body, sourceSection);
    }

    ISLISPProgn parseProgn(ParserContext parserContext, SourceSection sourceSection, Value body, boolean isTopLevel)  {
        var bodyStatements = new ArrayList<ISLISPExpressionNode>();
        for (var e: (Pair) body) {
            bodyStatements.add(parseExpressionNode(parserContext, e, isTopLevel));
        }
        return new ISLISPProgn(bodyStatements.toArray(ISLISPExpressionNode[]::new), sourceSection);
    }

    ISLISPFunctionRef parseFunctionRef(ParserContext parserContext, SourceSection sourceSection, Value rest) {
        var pair = (Pair) rest;
        var name = ((Symbol) pair.car()).name();
        var symbol = ISLISPContext.get(null).namedSymbol(name);
        return new ISLISPFunctionRef(symbol, sourceSection);
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

}

class IdGen {
    private int curr = 0;
    int next() {
        return curr++;
    }
}

class VariableContext {
    int frameDepth;
    int slot;
}

class ParserContext {
    final int frameDepth;
    final LexicalScope<SymbolReference, VariableContext> variables;
    final IdGen blocksIdGen;
    final LexicalScope<SymbolReference, Integer> blocks;

    ParserContext(int frameDepth, LexicalScope<SymbolReference, VariableContext> variables, IdGen blocksIdGen, LexicalScope<SymbolReference, Integer> blocks) {
        this.frameDepth = frameDepth;
        this.variables = variables;
        this.blocksIdGen = blocksIdGen;
        this.blocks = blocks;
    }

    ParserContext pushClosureScope() {
        return new ParserContext(frameDepth + 1, new LexicalScope<>(variables), blocksIdGen, blocks);
    }

    ParserContext pushBlockScope(SymbolReference blockName) {
        var newBlocks = new LexicalScope<>(blocks);
        newBlocks.put(blockName, blocksIdGen.next());
        return new ParserContext(frameDepth, variables, blocksIdGen, newBlocks);
    }

}