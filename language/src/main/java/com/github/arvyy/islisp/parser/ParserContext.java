package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.runtime.SymbolReference;
import com.oracle.truffle.api.frame.FrameDescriptor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Necessary parser context while doing recursive paring descent.
 * Needs to be public since it gets saved under debugger node to be reused during
 * inline parsing context from debugger.
 */
public class ParserContext {
    final String module;
    final int frameDepth;
    final LexicalScope<SymbolReference, VariableContext> variables;
    final LexicalScope<SymbolReference, VariableContext> localFunctions;
    final IdGen blocksIdGen;
    final LexicalScope<SymbolReference, Integer> blocks;
    final LexicalScope<SymbolReference, Integer> tagbodyTags;

    final FrameDescriptor.Builder frameBuilder;

    ParserContext(String module) {
        this(
                module,
                0,
                new LexicalScope<>(),
                new LexicalScope<>(),
                new IdGen(),
                new LexicalScope<>(),
                new LexicalScope<>(),
                FrameDescriptor.newBuilder());
    }

    ParserContext(
            String module,
            int frameDepth,
            LexicalScope<SymbolReference, VariableContext> variables,
            LexicalScope<SymbolReference, VariableContext> localFunctions,
            IdGen blocksIdGen,
            LexicalScope<SymbolReference, Integer> blocks,
            LexicalScope<SymbolReference, Integer> tagbodyTags,
            FrameDescriptor.Builder frameBuilder) {
        this.module = module;
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
                module,
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
                module,
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
                module,
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
                module,
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
                module,
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
                module,
                frameDepth,
                variables,
                localFunctions,
                blocksIdGen,
                blocks,
                tagbodyTags,
                FrameDescriptor.newBuilder());
    }

    static class VariableContext {
        String name;
        int frameDepth;
        int slot;
    }

    /**
     * Build local scope variable info from the context.
     *
     * @return a list, where each element represents a scope, wherein each scope is a list of variables it contains.
     * list are ordered from parent scope to child scope.
     *
     */
    public List<List<LocalScopeVariable>> getLocalScopeVariables() {
        var lst = new ArrayList<List<LocalScopeVariable>>();
        var lexicalScope = variables;
        while (lexicalScope != null) {
            var scopevars = new ArrayList<LocalScopeVariable>();
            for (var key: lexicalScope.listLocalKeys()) {
                var value = lexicalScope.get(key).get();
                var scopevar = new LocalScopeVariable(value.name, frameDepth - value.frameDepth, value.slot);
                scopevars.add(scopevar);
            }
            if (!scopevars.isEmpty()) {
                lst.add(scopevars);
            }
            lexicalScope = lexicalScope.getParent();
        }
        return lst;
    }

}
