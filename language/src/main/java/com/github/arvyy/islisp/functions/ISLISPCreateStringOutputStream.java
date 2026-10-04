package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.io.ByteArrayOutputStream;

/**
 * Implements `create-string-output-stream`.
 */
public class ISLISPCreateStringOutputStream extends ISLISPBuiltinFunctionRootNode {
    
    public ISLISPCreateStringOutputStream(TruffleLanguage<?> language) {
        super("create-string-output-stream", arg(0), language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    @CompilerDirectives.TruffleBoundary
    public Object executeImpl(VirtualFrame frame) {
        return new LispStream(null, new ByteArrayOutputStream());
    }
    
}
