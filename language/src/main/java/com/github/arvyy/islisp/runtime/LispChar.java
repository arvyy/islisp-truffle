package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

/**
 * Signifies LISP character type, carrying around codepoint.
 *
 * @param codepoint
 */
public record LispChar(int codepoint) implements TruffleObject {
}
