package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.source.SourceSection;

import java.util.List;

/**
 * An ISLISP class common interface -- classes are either builtin (primitive) or standard (created through defclass).
 */
public sealed interface LispClass permits StandardClass, BuiltinClass {

    /**
     *
     * @return list of superclasses
     */
    List<LispClass> getParents();

    /**
     *
     * @return if this class is abstract and cannot be instantiated directly
     */
    boolean isAbstract();

    /**
     * @return source location of class declaration.
     */
    SourceSection getSourceLocation();

}
