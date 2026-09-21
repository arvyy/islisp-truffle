package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * Implements `create-string-input-stream` method.
 */
public class ISLISPCreateStringInputStream extends RootNode {

    private final static String NAME = "create-string-input-stream";
    private final static List<String> ARGS = List.of("string");
    private final static String REST_ARG = null;
    private final static String RETURN_TYPE = "<stream>";
    private final static String DESC = "Creates and returns an input stream from the string. An error shall be signaled if string is not a string (error-id. domain-error).";
        
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
        return executeBoundary(value);
    }

    @CompilerDirectives.TruffleBoundary
    Object executeBoundary(CharSequence value) {
        return new LispStream(new ByteArrayInputStream(value.toString().getBytes(StandardCharsets.UTF_8)), null);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(
            new LispFunction.Documentation(NAME, ARGS, REST_ARG, RETURN_TYPE, DESC),
            new ISLISPCreateStringInputStream(lang).getCallTarget());
    }
}
