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
 * Implements `finish-output` function.
 */
public class ISLISPFinishOutput extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPFinishOutput(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length - 1,
                1,
                1
            );
        }
        if (frame.getArguments()[1] instanceof LispStream stream) {
            flush(stream);
            return ctx.getNil();
        }
        return errorSignalerNode.signalWrongType(frame.getArguments()[1], ctx.lookupClass("<stream>"));
    }

    @CompilerDirectives.TruffleBoundary
    void flush(LispStream out) {
        try {
            out.flush();
        } catch (IOException ignored) {
        }
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPFinishOutput(lang).getCallTarget());
    }
}
