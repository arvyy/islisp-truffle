package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.nodes.ControlFlowException;

/**
 * Exception used to implement (go tag) form to unwind to a tagbody section.
 *
 * @see com.github.arvyy.islisp.nodes.ISLISPTagBodyNode
 * @see com.github.arvyy.islisp.nodes.ISLISPTagBodyGoNode
 */
public class ISLISPTagbodyGoException extends ControlFlowException {

    private final int goId;

    /**
     * Create tagbody go exception with given target in the tagbody.
     *
     * @param goId unique target id
     */
    public ISLISPTagbodyGoException(int goId) {
        this.goId = goId;
    }

    /**
     * @return target id in the tagbody this exception is executing jumpt to.
     */
    public int getGoId() {
        return goId;
    }

}
