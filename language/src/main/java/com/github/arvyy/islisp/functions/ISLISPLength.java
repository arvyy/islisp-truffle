package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `length` function, return the size of a given sequence.
 */
public abstract class ISLISPLength extends RootNode {

    @Child
    ISLISPErrorSignalerNode islispErrorSignalerNode;

    ISLISPLength(TruffleLanguage<?> language) {
        super(language);
        islispErrorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        return executeGeneric(frame.getArguments()[1]);
    }

    abstract Object executeGeneric(Object obj);

    @Specialization
    Object doVector(LispVector vector) {
        return vector.values().length;
    }

    @Specialization
    Object doString(String str) {
        return str.length();
    }

    @Specialization
    Object doMutableString(LispMutableString s) {
        return s.chars().length;
    }

    @Specialization
    Object doSymbol(Symbol s) {
        if (s.identityReference() == ISLISPContext.get(this).getNil().identityReference()) {
            return 0;
        } else {
            var ctx = ISLISPContext.get(this);
            return islispErrorSignalerNode.signalDomainError("Not a sequence", s, ctx.lookupClass("<list>"));
        }
    }

    @Specialization
    Object doPair(Pair p) {
        Object v = p;
        int len = 0;
        while (v instanceof Pair pair) {
            len++;
            v = pair.cdr();
        }
        return len;
    }

    @Specialization(guards = {
        "interop.hasArrayElements(o)"
    }, limit = "3")
    Object doTruffleVector(
        Object o,
        @CachedLibrary("o") InteropLibrary interop
    ) {
        try {
            return (int) interop.getArraySize(o);
        } catch (UnsupportedMessageException e) {
            return islispErrorSignalerNode.signalTruffleInteropError(e);
        }
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPLengthNodeGen.create(lang).getCallTarget());
    }

}
