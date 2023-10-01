package com.github.arvyy.islisp.parser;

public class EqWrapper {

    private final Object value;


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
