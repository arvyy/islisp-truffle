package com.github.arvyy.islisp.exceptions;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;

/**
 * Generic WIP error that isn't a condition.
 */
public class ISLISPError extends AbstractTruffleException {

    /**
     * Create error exception.
     *
     * @param message message
     * @param node initiation node
     */
    public ISLISPError(String message, Node node) {
        super(message, node);
    }

}
