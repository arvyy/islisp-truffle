package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.io.IOException;

/**
 * Implements `close` function.
 */
public class ISLISPClose extends ISLISPBuiltinFunctionRootNode {
    
    public ISLISPClose(TruffleLanguage<?> language) {
        super("close", arg(1), language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object executeImpl(VirtualFrame frame) {
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


}
