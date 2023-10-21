package com.github.arvyy.islisp.parser;

/**
 * Helper class to wrap object and use reference equality and identity hashcode
 * instead of class'es override. Used in Map for source code.
 */
public class EqWrapper {

    private final Object value;


    /**
     * Create eq wrapper around given object.
     *
     * @param value wrapped object
     */
    public EqWrapper(Object value) {
        this.value = value;
    }

    @Override
    public int hashCode() {
        return System.identityHashCode(value);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof EqWrapper other) {
            return value == other.value;
        }
        return false;
    }
}
