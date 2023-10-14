package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.exception.AbstractTruffleException;

public class ISLISPNonContinuableCondition extends AbstractTruffleException {

    private final Object condition;

    public ISLISPNonContinuableCondition(Object condition) {
        this.condition = condition;
    }

    public Object getCondition() {
        return condition;
    }
}
