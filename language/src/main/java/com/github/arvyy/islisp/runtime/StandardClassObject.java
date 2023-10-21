package com.github.arvyy.islisp.runtime;

/**
 * Represents instances of a standard class.
 * @param clazz object's class
 * @param data object's data (created through truffle shape factory)
 */
public record StandardClassObject(StandardClass clazz, Object data) { }
