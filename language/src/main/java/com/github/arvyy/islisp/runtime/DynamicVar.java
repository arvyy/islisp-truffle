package com.github.arvyy.islisp.runtime;

public class DynamicVar {

    private Value value;

    public Value getValue() {
        return value;
    }

    public void setValue(Value v) {
        value = v;
    }

}
