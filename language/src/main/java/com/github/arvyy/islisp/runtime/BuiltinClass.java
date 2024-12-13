package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.source.SourceSection;

import java.util.List;

/**
 * Presents a builtin class whose instances
 * are specially handled and which cannot be subclassed by users.
 */
public final class BuiltinClass implements LispClass, TruffleObject {

    private final List<LispClass> parents;
    private final Symbol name;
    private final boolean isAbstract;

    /**
     * Create builtin class.
     *
     * @param parents list of parents
     * @param name class name
     * @param isAbstract if this class cannot be instantiated directly
     */
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

    /**
     * @return class name.
     */
    public String name() {
        return name.name();
    }

    @Override
    public SourceSection getSourceLocation() {
        return null;
    }
}
