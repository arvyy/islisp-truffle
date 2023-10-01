package com.github.arvyy.islisp.runtime;

public class ValueReference {

    private Object value;
    private boolean readOnly;

    public Object getValue() {
        return value;
    }

    public void setValue(Object v) {
        value = v;
    }

    public boolean isReadOnly() {
        return readOnly;
    }

    public void setReadOnly(boolean readOnly) {
        this.readOnly = readOnly;
    }
}
