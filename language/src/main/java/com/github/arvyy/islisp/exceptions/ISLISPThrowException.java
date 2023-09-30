package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.nodes.ControlFlowException;

public class ISLISPThrowException extends ControlFlowException {

    final Object catchTag;
    final Object result;

    public ISLISPThrowException(Object catchTag, Object result) {
        this.catchTag = catchTag;
        this.result = result;
    }

    public Object getCatchTag() {
        return catchTag;
    }

    public Object getResult() {
        return result;
    }
}
