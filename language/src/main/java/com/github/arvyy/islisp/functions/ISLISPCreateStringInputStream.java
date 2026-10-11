package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

/**
 * Implements `create-string-input-stream` method.
 */
public class ISLISPCreateStringInputStream extends ISLISPBuiltinFunctionRootNode {

    /**
    * Construct node.
    * @param language truffle language reference
    */
    public ISLISPCreateStringInputStream(TruffleLanguage<?> language) {
        super("create-string-input-stream", arg(1), language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object executeImpl(VirtualFrame frame) {
        var arg = frame.getArguments()[1];
        CharSequence value;
        if (arg instanceof String s) {
            value = s;
        } else if (arg instanceof StringBuffer s) {
            value = s;
        } else {
            var ctx = ISLISPContext.get(this);
            return errorSignalerNode.signalWrongType(arg, ctx.lookupClass("<string>"));
        }
        return executeBoundary(value);
    }

    @CompilerDirectives.TruffleBoundary
    Object executeBoundary(CharSequence value) {
        return new LispStream(new ByteArrayInputStream(value.toString().getBytes(StandardCharsets.UTF_8)), null);
    }

}
