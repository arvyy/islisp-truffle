package com.github.arvyy.islisp;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;

public class ISLISPError extends AbstractTruffleException {

    public ISLISPError(String message, Node node) {
        super(message, node);
    }

}
