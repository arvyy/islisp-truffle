package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

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
    LispStream executeBoundary() {
        try {
            return new LispStream(null, new ByteArrayOutputStream());
        } catch (IOException e) {
            //TODO
            throw new ISLISPError(e.getMessage(), this);
        }
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
