package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.exception.AbstractTruffleException;

/**
 * Exception thrown in case a condition is signalled, but no
 * signal handler is present (can happen if an islisp function is stored and reinvoked from non-islisp context).
 */
public class ISLISPUncaughtConditionException extends AbstractTruffleException {

    private final Object condition;

    /**
     * Create non-continuable condition signalling exception.
     *
     * @param condition condition value
     */
    public ISLISPUncaughtConditionException(Object condition) {
        this.condition = condition;
    }

    /**
     * @return condition value
     */
    public Object getCondition() {
        return condition;
    }

}
