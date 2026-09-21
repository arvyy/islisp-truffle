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
import java.util.List;

/**
 * Implements `close` function.
 */
public class ISLISPClose extends RootNode {
    
    private final static String NAME = "close";
    private final static List<String> ARGS = List.of("stream");
    private final static String REST_ARG = null;
    private final static String RETURN_TYPE = null;
    private final static String DESC = "The function close closes the stream stream. If stream is closed it may no longer be used in input or output operations. Closing a file stream ends the association between the stream and its file. If the stream was already closed this function performs nothing. The result value is implementation defined. An error shall be signaled if stream is not a stream (error-id. domain-error).";

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
        return new LispFunction(
            new LispFunction.Documentation(NAME, ARGS, REST_ARG, RETURN_TYPE, DESC),
            new ISLISPClose(lang).getCallTarget());
    }


}
