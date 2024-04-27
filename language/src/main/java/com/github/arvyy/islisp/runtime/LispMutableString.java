package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

/**
 * Represents a mutable islisp string.
 * @param chars string content.
 */
public record LispMutableString(LispChar[] chars) implements TruffleObject {
}
