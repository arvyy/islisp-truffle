package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

/**
 * Represents instances of a standard class.
 * @param clazz object's class
 * @param data object's data (created through truffle shape factory)
 */
public record StandardClassObject(StandardClass clazz, Object data) implements TruffleObject  { }
