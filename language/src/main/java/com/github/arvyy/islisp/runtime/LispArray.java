package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

/**
 * Lisp array.
 *
 * @param data nested Object[] values
 * @param dimensions amount of nesting; 2+.
 */
public record LispArray(Object[] data, int dimensions) implements TruffleObject {
}
