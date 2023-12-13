package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

/**
 * Represents loaded native library.
 * @param nativeLibrary truffle interop object from internal nfi language.
 */
public record LispNativeLibrary(Object nativeLibrary) implements TruffleObject {
}
