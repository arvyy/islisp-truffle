package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.nodes.ControlFlowException;

/**
 * Exception used to implement (return-from) form to unwind to specified block
 */
public class ISLISPReturnFromException extends ControlFlowException {

    private final int blockId;
    private final Object value;

    public ISLISPReturnFromException(int blockId, Object value) {
        this.blockId = blockId;
        this.value = value;
    }

    public int getBlockId() {
        return blockId;
    }

    public Object getValue() {
        return value;
    }
}
