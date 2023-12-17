package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.exception.AbstractTruffleException;

/**
 * Exception to exit evaluation when run in interactive context.
 */
public class ISLISPInteractiveExitException extends AbstractTruffleException {

    private final Object condition;

    /**
     * @return uncaught condition that caused the exit.
     */
    public Object getCondition() {
        return condition;
    }

    /**
     * Create exit exception.
     * @param condition islisp uncaught condition cause.
     */
    public ISLISPInteractiveExitException(Object condition) {
        this.condition = condition;
    }
}
