package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispOutputStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.ByteArrayOutputStream;

/**
 * Implements `create-string-output-stream`.
 */
public class ISLISPCreateStringOutputStream extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPCreateStringOutputStream(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 1) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 0, 0);
        }
        return executeBoundary();
    }

    @CompilerDirectives.TruffleBoundary
    LispOutputStream executeBoundary() {
        return new LispOutputStream(new ByteArrayOutputStream());
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPCreateStringOutputStream(lang).getCallTarget());
    }
}
