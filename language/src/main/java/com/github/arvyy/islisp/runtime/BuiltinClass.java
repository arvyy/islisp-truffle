package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.source.SourceSection;

import java.util.List;

public final class BuiltinClass implements LispClass {

    private final List<LispClass> parents;
    private final Symbol name;

    public BuiltinClass(List<LispClass> parents, Symbol name) {
        this.parents = parents;
        this.name = name;
    }

    @Override
    public List<LispClass> getParents() {
        return parents;
    }

    @Override
    public SourceSection sourceSection() {
        return null;
    }

}
