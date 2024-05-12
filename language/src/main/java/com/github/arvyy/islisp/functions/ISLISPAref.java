package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `aref` function.
 */
public abstract class ISLISPAref extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPAref(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length < 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length, 2, -1);
        }
        int[] lookup = new int[frame.getArguments().length - 2];
        for (var i = 0; i < lookup.length; i++) {
            var arg = frame.getArguments()[i + 2];
            if (arg instanceof Integer) {
                lookup[i] = (int) arg;
            } else if (arg instanceof LispBigInteger) {
                lookup[i] = bigintValue((LispBigInteger) arg);
            } else {
                var ctx = ISLISPContext.get(this);
                return errorSignalerNode.signalWrongType(arg, ctx.lookupClass("<integer>"));
            }
        }
        return executeGeneric(frame.getArguments()[1], lookup);
    }

    @CompilerDirectives.TruffleBoundary
    int bigintValue(LispBigInteger arg) {
        return arg.data().intValueExact();
    }

    abstract Object executeGeneric(Object array, int[] lookup);

    @Specialization
    Object executeArray(LispArray arr, int[] lookup) {
        if (arr.dimensions() != lookup.length) {
            return errorSignalerNode.signalDomainError(
                "Expected dimension size " + arr.dimensions() + "; received" + lookup.length,
                arr,
                ISLISPContext.get(this).lookupClass("<basic-array*>"));
        }
        Object obj = arr.data();
        for (var i: lookup) {
            var subobj = (Object[]) obj;
            if (i < 0 || i >= subobj.length) {
                return errorSignalerNode.signalIndexOutOfRange(i, subobj.length);
            }
            obj = subobj[i];
        }
        return obj;
    }

    @Specialization
    Object executeVector(LispVector vec, int[] lookup) {
        if (lookup.length != 1) {
            return errorSignalerNode.signalDomainError(
                "Expected dimension size 1; received" + lookup.length,
                vec,
                ISLISPContext.get(this).lookupClass("<basic-vector>"));
        }
        var index = lookup[0];
        if (index < 0 || index >= vec.values().length) {
            return errorSignalerNode.signalIndexOutOfRange(index, vec.values().length);
        }
        return vec.values()[index];
    }

    @Specialization
    Object executeString(String s, int[] lookup) {
        if (lookup.length != 1) {
            return errorSignalerNode.signalDomainError(
                "Expected dimension size 1; received" + lookup.length,
                s,
                ISLISPContext.get(this).lookupClass("<string>"));
        }
        var index = lookup[0];
        if (index < 0 || index >= s.length()) {
            return errorSignalerNode.signalIndexOutOfRange(index, s.length());
        }
        var c = s.codePointAt(index);
        return new LispChar(c);
    }

    @Specialization
    Object executeMutableString(LispMutableString s, int[] lookup) {
        if (lookup.length != 1) {
            return errorSignalerNode.signalDomainError(
                "Expected dimension size 1; received" + lookup.length,
                s,
                ISLISPContext.get(this).lookupClass("<string>"));
        }
        var index = lookup[0];
        if (index < 0 || index >= s.chars().length) {
            return errorSignalerNode.signalIndexOutOfRange(index, s.chars().length);
        }
        return s.chars()[index];
    }

    @Fallback
    Object fallback(Object arr, int[] lookup) {
        var ctx = ISLISPContext.get(this);
        return errorSignalerNode.signalWrongType(arr, ctx.lookupClass("<basic-array>"));
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPArefNodeGen.create(lang).getCallTarget());
    }

}
