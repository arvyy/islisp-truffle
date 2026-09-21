package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.ByteArrayOutputStream;
import java.util.List;

/**
 * Implements `create-string-output-stream`.
 */
public class ISLISPCreateStringOutputStream extends RootNode {

    private final static String NAME = "create-string-output-stream";
    private final static List<String> ARGS = List.of();
    private final static String REST_ARG = null;
    private final static String RETURN_TYPE = "<stream>";
    private final static String DESC = "This function creates and returns a string output stream. The output to a string stream can be retrieved by get-output-stream-string.";
    
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
        return new LispStream(null, new ByteArrayOutputStream());
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(
            new LispFunction.Documentation(NAME, ARGS, REST_ARG, RETURN_TYPE, DESC),
            new ISLISPCreateStringOutputStream(lang).getCallTarget());
    }
}
