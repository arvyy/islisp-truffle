package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.exception.AbstractTruffleException;

/**
 * Exception to exit evaluation when run in interactive context
 */
public class ISLISPInteractiveExitException extends AbstractTruffleException {

    private final Object condition;

    public Object getCondition() {
        return condition;
    }

    public ISLISPInteractiveExitException(Object condition) {
        this.condition = condition;
    }
}
