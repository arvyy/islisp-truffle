package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.exception.AbstractTruffleException;

/**
 * Control flow exception that unwinds stack from point of signal
 * to the handler for non-continuable conditions.
 *
 * See
 * @{@link com.github.arvyy.islisp.functions.ISLISPSignalCondition},
 * @{@link com.github.arvyy.islisp.nodes.ISLISPWithHandlerNode}.
 */
public class ISLISPNonContinuableCondition extends AbstractTruffleException {

    private final Object condition;

    /**
     * Create non-continuable condition signalling exception.
     *
     * @param condition condition value
     */
    public ISLISPNonContinuableCondition(Object condition) {
        this.condition = condition;
    }

    /**
     * @return condition value
     */
    public Object getCondition() {
        return condition;
    }
}
