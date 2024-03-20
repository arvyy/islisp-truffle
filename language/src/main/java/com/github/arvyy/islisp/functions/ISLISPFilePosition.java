package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispBigInteger;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.IOException;
import java.math.BigInteger;

/**
 * Implements `file-position` function.
 */
public abstract class ISLISPFilePosition extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPFilePosition(TruffleLanguage<?> language) {
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

    abstract Object executeGeneric(Object obj);

    @Specialization
    Object doProper(LispStream stream) {
        if (stream.isFileBased()) {
            try {
                return new LispBigInteger(BigInteger.valueOf(stream.getFilePosition()));
            } catch (IOException e) {
                throw new ISLISPError(e.getMessage(), this);
            }
        }
        throw new ISLISPError("file-position called on non-file stream", this);
    }

    @Fallback
    Object doFallback(Object obj) {
        return errorSignalerNode.signalWrongType(obj, ISLISPContext.get(this).lookupClass("<stream>"));
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPFilePositionNodeGen.create(lang).getCallTarget());
    }

}
