package com.github.arvyy.islisp.runtime;

import java.util.List;

public final class BuiltinClass implements LispClass {

    private final List<LispClass> parents;
    private final Symbol name;
    private final boolean isAbstract;

    public BuiltinClass(List<LispClass> parents, Symbol name, boolean isAbstract) {
        this.parents = parents;
        this.name = name;
        this.isAbstract = isAbstract;
    }

    @Override
    public List<LispClass> getParents() {
        return parents;
    }

    @Override
    public boolean isAbstract() {
        return isAbstract;
    }
}
