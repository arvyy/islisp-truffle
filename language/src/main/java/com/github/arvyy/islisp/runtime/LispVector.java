package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

/**
 * Lisp vector.
 * @param values vector content
 */
public record LispVector(Object[] values) implements TruffleObject { }
