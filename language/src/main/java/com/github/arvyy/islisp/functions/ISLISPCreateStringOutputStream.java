package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.io.ByteArrayOutputStream;

/**
 * Implements `create-string-output-stream`.
 */
public class ISLISPCreateStringOutputStream extends ISLISPBuiltinFunctionRootNode {

    /**
    * Construct node.
    * @param language truffle language reference
    */
    public ISLISPCreateStringOutputStream(TruffleLanguage<?> language) {
        super("create-string-output-stream", arg(0), language);
    }

    @Override
    public Object executeImpl(VirtualFrame frame) {
        return executeBoundary();
    }

    @CompilerDirectives.TruffleBoundary
    Object executeBoundary() {
        return new LispStream(null, new ByteArrayOutputStream());
    }

}
