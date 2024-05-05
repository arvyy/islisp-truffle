package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.IOException;

/**
 * Implements `format-fresh-line` function.
 */
public abstract class ISLISPFormatFreshLine extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPFormatFreshLine(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 1, 1);
        }
        return executeGeneric(frame.getArguments()[1]);
    }

    abstract Object executeGeneric(Object stream);

    @Specialization
    Object executeProper(LispStream stream) {
        return doPrint(stream);
    }

    @Fallback
    Object executeFallback(Object stream) {
        return errorSignalerNode.signalWrongType(stream, ISLISPContext.get(this).lookupClass("<stream>"));
    }


    @CompilerDirectives.TruffleBoundary
    Object doPrint(LispStream stream) {
        try {
            stream.write("\n");
            stream.flush();
            return ISLISPContext.get(this).getNil();
        } catch (IOException e) {
            return errorSignalerNode.signalIOError(e);
        }
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPFormatFreshLineNodeGen.create(lang).getCallTarget());
    }
}
