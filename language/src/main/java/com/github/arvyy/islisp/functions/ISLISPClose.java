package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.IOException;

/**
 * Implements `close` function.
 */
public class ISLISPClose extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPClose(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 1, 1);
        }
        var arg = frame.getArguments()[1];
        var ctx = ISLISPContext.get(this);
        if (arg instanceof LispStream stream) {
            close(stream);
            return ctx.getNil();
        }
        return errorSignalerNode.signalWrongType(arg, ctx.lookupClass("<stream>"));
    }

    @CompilerDirectives.TruffleBoundary
    void close(LispStream stream) {
        try {
            stream.close();
        } catch (IOException ignored) {
            //TODO log something somehow?
            //spec doesn't specify about any conditions here.
        }
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPClose(lang).getCallTarget());
    }


}
