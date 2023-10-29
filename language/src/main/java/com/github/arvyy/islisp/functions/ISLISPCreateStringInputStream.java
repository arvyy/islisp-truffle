package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispInputStream;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;

/**
 * Implements `create-string-input-stream` method.
 */
public class ISLISPCreateStringInputStream extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPCreateStringInputStream(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 1, 1);
        }
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
        try {
            return new LispInputStream(new ByteArrayInputStream(value.toString().getBytes("UTF-8")));
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPCreateStringInputStream(lang).getCallTarget());
    }
}
