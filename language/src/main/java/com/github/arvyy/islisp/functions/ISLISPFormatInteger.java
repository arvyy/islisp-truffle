package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.nodes.ISLISPTypes;
import com.github.arvyy.islisp.runtime.LispBigInteger;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispStream;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.io.IOException;

/**
 * Implements `format-integer` function, that writes a given integer to output stream.
 */
@TypeSystemReference(ISLISPTypes.class)
public abstract class ISLISPFormatInteger extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPFormatInteger(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    abstract Object executeGeneric(Object stream, Object integer, Object radix);

    @Override
    public final Object execute(VirtualFrame frame) {
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2], frame.getArguments()[3]);
    }

    @Specialization
    Object doProper(LispStream stream, int integer, int radix) {
        return doPrint(stream, integer, radix);
    }

    @Specialization
    @CompilerDirectives.TruffleBoundary
    Object doProper(LispStream stream, LispBigInteger integer, LispBigInteger radix) {
        return doPrint(stream, integer.data().intValue(), radix.data().intValue());
    }

    @Specialization
    Object doFallbackBadStream(Object stream, LispBigInteger integer, LispBigInteger radix) {
        return errorSignalerNode.signalWrongType(stream, ISLISPContext.get(this).lookupClass("<stream>"));
    }

    @Specialization
    Object doFallbackBadInteger(Object stream, Object integer, LispBigInteger radix) {
        return errorSignalerNode.signalWrongType(integer, ISLISPContext.get(this).lookupClass("<integer>"));
    }

    @Specialization
    Object doFallbackBadRadix(Object stream, Object integer, Object radix) {
        return errorSignalerNode.signalWrongType(radix, ISLISPContext.get(this).lookupClass("<integer>"));
    }


    @CompilerDirectives.TruffleBoundary
    Object doPrint(LispStream s, int value, int radix) {
        try {
            s.write(Integer.toString(value, radix).toUpperCase());
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
        return new LispFunction(ISLISPFormatIntegerNodeGen.create(lang).getCallTarget());
    }
}
