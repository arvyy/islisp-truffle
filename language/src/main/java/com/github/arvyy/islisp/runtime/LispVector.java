package com.github.arvyy.islisp.runtime;

/**
 * Lisp vector.
 * @param values vector content
 */
//TODO remove and use Object[] directly?
public record LispVector(Object[] values) { }
