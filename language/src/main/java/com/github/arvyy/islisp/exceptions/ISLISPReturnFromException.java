package com.github.arvyy.islisp.exceptions;

import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.nodes.ControlFlowException;

public class ISLISPReturnFromException extends ControlFlowException {

    private final int blockId;
    private final Value value;

    public ISLISPReturnFromException(int blockId, Value value) {
        this.blockId = blockId;
        this.value = value;
    }

    public int getBlockId() {
        return blockId;
    }

    public Value getValue() {
        return value;
    }
}
