package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.nodes.ControlFlowException;

/**
 * Exception used to unwind stack from (throw) statement back to
 * nearest (catch).
 *
 * See
 * @see com.github.arvyy.islisp.nodes.ISLISPCatchNode
 * @see com.github.arvyy.islisp.nodes.ISLISPThrowNode
 */
public class ISLISPThrowException extends ControlFlowException {

    final Object catchTag;
    final Object result;

    /**
     * Create throw exception to unwind to corresponding catch node.
     *
     * @param catchTag tag which must be equal to catch's tag
     * @param result returned value
     */
    public ISLISPThrowException(Object catchTag, Object result) {
        this.catchTag = catchTag;
        this.result = result;
    }

    /**
     * @return catch tag
     */
    public Object getCatchTag() {
        return catchTag;
    }

    /**
     * @return returned catch value
     */
    public Object getResult() {
        return result;
    }
}
