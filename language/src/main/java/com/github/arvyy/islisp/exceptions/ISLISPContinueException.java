package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.nodes.ControlFlowException;

/**
 * Control flow exception that unwinds condition handler back
 * to the signal call.
 *
 * See
 * @see com.github.arvyy.islisp.functions.ISLISPSignalCondition
 * @see com.github.arvyy.islisp.nodes.ISLISPWithHandlerNode
 */
public class ISLISPContinueException extends ControlFlowException {

    private final Object condition;
    private final Object value;

    /**
     * Create signal continue exception.
     *
     * @param condition condition object
     * @param value value to be used from signal-condition expression
     */
    public ISLISPContinueException(Object condition, Object value) {
        this.condition = condition;
        this.value = value;
    }

    /**
     * @return condition object
     */
    public Object getCondition() {
        return condition;
    }

    /**
     * @return value to be used from signal-condition expression
     */
    public Object getValue() {
        return value;
    }
}
