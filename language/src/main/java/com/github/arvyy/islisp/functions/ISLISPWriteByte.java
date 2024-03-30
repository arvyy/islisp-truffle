package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
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

/**
 * Implements `write-byte` function.
 */
public abstract class ISLISPWriteByte extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPWriteByte(TruffleLanguage<?> language) {
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

    abstract Object executeGeneric(Object b, Object stream);

    @Specialization(guards = {
        "stream.hasOutput()"
    })
    Object doInt(int b, LispStream stream) {
        if (b < 0 || b > 255) {
            var intClass = ISLISPContext.get(this).lookupClass("<integer>");
            return errorSignalerNode.signalDomainError(
                "Given byte value out of bounds, it must be between 0 and 255",
                b,
                intClass);
        }
        writeBoundary(b, stream);
        return ISLISPContext.get(this).getNil();
    }

    @Specialization(guards = {
        "stream.hasOutput()"
    })
    @CompilerDirectives.TruffleBoundary
    Object doBigInt(LispBigInteger bigInteger, LispStream stream) {
        int b = bigInteger.data().intValueExact();
        return doInt(b, stream);
    }

    @Fallback
    Object fallback(Object num, Object stream) {
        var ctx = ISLISPContext.get(this);
        return errorSignalerNode.signalWrongType(stream, ctx.lookupClass("<stream>"));
    }

    @CompilerDirectives.TruffleBoundary
    void writeBoundary(int b, LispStream stream) {
        try {
            stream.writeByte(b);
        } catch (IOException e) {
            throw new ISLISPError(e.getMessage(), this);
        }
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPWriteByteNodeGen.create(lang).getCallTarget());
    }


}
