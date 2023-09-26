package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.runtime.SymbolReference;
import com.oracle.truffle.api.frame.FrameDescriptor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

class ParserContext {
    final int frameDepth;
    final LexicalScope<SymbolReference, VariableContext> variables;
    final LexicalScope<SymbolReference, VariableContext> localFunctions;
    final IdGen blocksIdGen;
    final LexicalScope<SymbolReference, Integer> blocks;
    final LexicalScope<SymbolReference, Integer> tagbodyTags;

    final FrameDescriptor.Builder frameBuilder;

    ParserContext() {
        this(
                0,
                new LexicalScope<>(),
                new LexicalScope<>(),
                new IdGen(),
                new LexicalScope<>(),
                new LexicalScope<>(),
                FrameDescriptor.newBuilder());
    }

    ParserContext(
            int frameDepth,
            LexicalScope<SymbolReference, VariableContext> variables,
            LexicalScope<SymbolReference, VariableContext> localFunctions,
            IdGen blocksIdGen,
            LexicalScope<SymbolReference, Integer> blocks,
            LexicalScope<SymbolReference, Integer> tagbodyTags,
            FrameDescriptor.Builder frameBuilder) {
        this.frameDepth = frameDepth;
        this.variables = variables;
        this.localFunctions = localFunctions;
        this.blocksIdGen = blocksIdGen;
        this.blocks = blocks;
        this.frameBuilder = frameBuilder;
        this.tagbodyTags = tagbodyTags;
    }

    ParserContext pushClosureScope() {
        return new ParserContext(
                frameDepth + 1,
                variables,
                localFunctions,
                blocksIdGen,
                blocks,
                tagbodyTags,
                frameBuilder);
    }

    ParserContext pushLexicalScope(Map<SymbolReference, VariableContext> vars) {
        return new ParserContext(
                frameDepth,
                new LexicalScope<>(variables, vars),
                localFunctions,
                blocksIdGen,
                blocks,
                tagbodyTags,
                frameBuilder);
    }

    ParserContext pushLexicalFunctionScope(Map<SymbolReference, VariableContext> vars) {
        return new ParserContext(
                frameDepth,
                variables,
                new LexicalScope<>(localFunctions, vars),
                blocksIdGen,
                blocks,
                tagbodyTags,
                frameBuilder);
    }

    ParserContext pushBlockScope(SymbolReference blockName) {
        var newBlocks = new LexicalScope<>(blocks, Map.of(blockName, blocksIdGen.next()));
        return new ParserContext(
                frameDepth,
                variables,
                localFunctions,
                blocksIdGen,
                newBlocks,
                tagbodyTags,
                frameBuilder);
    }

    ParserContext pushTagbodyScope(List<SymbolReference> tags) {
        var map = new HashMap<SymbolReference, Integer>();
        for (var tag: tags) {
            map.put(tag, blocksIdGen.next());
        }
        var newTagbodyTags = new LexicalScope<>(tagbodyTags, map);
        return new ParserContext(
                frameDepth,
                variables,
                localFunctions,
                blocksIdGen,
                blocks,
                newTagbodyTags,
                frameBuilder);
    }

    ParserContext pushFrameDescriptor() {
        return new ParserContext(
                frameDepth,
                variables,
                localFunctions,
                blocksIdGen,
                blocks,
                tagbodyTags,
                FrameDescriptor.newBuilder());
    }

    static class VariableContext {
        int frameDepth;
        int slot;
    }
}
