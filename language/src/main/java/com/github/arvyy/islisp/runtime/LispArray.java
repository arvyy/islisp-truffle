package com.github.arvyy.islisp.runtime;

/**
 * Lisp array.
 *
 * @param data nested Object[] values
 * @param dimensions amount of nesting; 2+.
 */
public record LispArray(Object[] data, int dimensions) {
}
