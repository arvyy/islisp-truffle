package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.nodes.ControlFlowException;

public class ISLISPContinueException extends ControlFlowException {

    private final Object condition;
    private final Object value;

    public ISLISPContinueException(Object condition, Object value) {
        this.condition = condition;
        this.value = value;
    }

    public Object getCondition() {
        return condition;
    }

    public Object getValue() {
        return value;
    }
}
