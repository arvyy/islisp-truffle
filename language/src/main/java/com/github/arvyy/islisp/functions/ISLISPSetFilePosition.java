package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispBigInteger;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
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
public abstract class ISLISPSetFilePosition extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPSetFilePosition(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 2, 2);
        }
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    abstract Object executeGeneric(Object obj, Object position);

    @Specialization
    @CompilerDirectives.TruffleBoundary
    Object doProper(LispStream stream, int position) {
        if (stream.isFileBased()) {
            try {
                stream.setFilePosition(position);
                return new LispBigInteger(BigInteger.valueOf(stream.getFilePosition()));
            } catch (IOException e) {
                return errorSignalerNode.signalIOError(e);
            }
        }
        return errorSignalerNode.signalDomainError(
            "set-file-position called on non-file stream",
            stream,
            ISLISPContext.get(this).lookupClass("<stream>"));
    }

    @Specialization
    @CompilerDirectives.TruffleBoundary
    Object doProper(LispStream stream, LispBigInteger position) {
        if (stream.isFileBased()) {
            try {
                stream.setFilePosition(position.data().longValue());
                return new LispBigInteger(BigInteger.valueOf(stream.getFilePosition()));
            } catch (IOException e) {
                return errorSignalerNode.signalIOError(e);
            }
        }
        return errorSignalerNode.signalDomainError(
            "set-file-position called on non-file stream",
            stream,
            ISLISPContext.get(this).lookupClass("<stream>"));
    }

    @Fallback
    Object doFallback(Object stream, Object position) {
        if (stream instanceof LispStream) {
            return errorSignalerNode.signalWrongType(position, ISLISPContext.get(this).lookupClass("<integer>"));
        } else {
            return errorSignalerNode.signalWrongType(stream, ISLISPContext.get(this).lookupClass("<stream>"));
        }
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPSetFilePositionNodeGen.create(lang).getCallTarget());
    }

}
