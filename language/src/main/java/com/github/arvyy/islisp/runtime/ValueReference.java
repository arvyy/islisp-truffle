package com.github.arvyy.islisp.runtime;

public class ValueReference {

    private Value value;
    private boolean readOnly;

    public Value getValue() {
        return value;
    }

    public void setValue(Value v) {
        value = v;
    }

    public boolean isReadOnly() {
        return readOnly;
    }

    public void setReadOnly(boolean readOnly) {
        this.readOnly = readOnly;
    }
}
