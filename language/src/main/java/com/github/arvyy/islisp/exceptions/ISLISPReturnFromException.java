package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.nodes.ControlFlowException;

/**
 * Exception used to implement (return-from) form to unwind to specified block.
 * Exception bubbles up until a block node is found with corresponding id.
 *
 * See
 * @see com.github.arvyy.islisp.nodes.ISLISPBlockNode
 * @see com.github.arvyy.islisp.nodes.ISLISPReturnFromNode
 */
public class ISLISPReturnFromException extends ControlFlowException {

    private final int blockId;
    private final Object value;

    /**
     * Create return from exception.
     *
     * @param blockId id of the block to unwind to
     * @param value value to use as a result of block expression
     */
    public ISLISPReturnFromException(int blockId, Object value) {
        this.blockId = blockId;
        this.value = value;
    }

    /**
     * @return id of the block to unwind to
     */
    public int getBlockId() {
        return blockId;
    }

    /**
     * @return value to use as a result of block expression
     */
    public Object getValue() {
        return value;
    }
}
