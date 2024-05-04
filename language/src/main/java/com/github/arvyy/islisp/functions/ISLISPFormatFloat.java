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
 * Implements `format-float` function.
 */
public abstract class ISLISPFormatFloat extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPFormatFloat(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    abstract Object executeGeneric(Object stream, Object f);

    @Override
    public final Object execute(VirtualFrame frame) {
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    @Specialization
    Object doProper(LispStream stream, double f) {
        return doPrint(stream, f);
    }

    @Specialization
    Object doFallbackNonStream(Object stream, double f) {
        return errorSignalerNode.signalWrongType(stream, ISLISPContext.get(this).lookupClass("<stream>"));
    }

    @Fallback
    Object doFallbackNonFloat(Object stream, Object f) {
        return errorSignalerNode.signalWrongType(f, ISLISPContext.get(this).lookupClass("<float>"));
    }


    @CompilerDirectives.TruffleBoundary
    Object doPrint(LispStream stream, double f) {
        try {
            stream.write(Double.toString(f));
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
        return new LispFunction(ISLISPFormatFloatNodeGen.create(lang).getCallTarget());
    }
}
