package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.nodes.ControlFlowException;

public class ISLISPTagbodyGoException extends ControlFlowException {

    private final int goId;

    public ISLISPTagbodyGoException(int goId) {
        this.goId = goId;
    }

    public int getGoId() {
        return goId;
    }

}
