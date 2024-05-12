package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `set-aref` function.
 */
public abstract class ISLISPSetAref extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPSetAref(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length < 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length, 2, -1);
        }
        int[] lookup = new int[frame.getArguments().length - 3];
        for (var i = 0; i < lookup.length; i++) {
            var arg = frame.getArguments()[i + 3];
            if (arg instanceof Integer) {
                lookup[i] = (int) arg;
            } else if (arg instanceof LispBigInteger) {
                lookup[i] = bigIntValue((LispBigInteger) arg);
            } else {
                var ctx = ISLISPContext.get(this);
                return errorSignalerNode.signalWrongType(arg, ctx.lookupClass("<integer>"));
            }
        }
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2], lookup);
    }

    @CompilerDirectives.TruffleBoundary
    int bigIntValue(LispBigInteger arg) {
        return arg.data().intValueExact();
    }

    abstract Object executeGeneric(Object value, Object array, int[] lookup);

    @Specialization
    Object doArray(Object value, LispArray arr, int[] lookup) {
        if (arr.dimensions() != lookup.length) {
            return signalWrongDimension(arr, arr.dimensions(), lookup.length);
        }
        Object[] obj = arr.data();
        for (var i = 0; i < lookup.length - 1; i++) {
            var index = lookup[i];
            if (index < 0 || index >= obj.length) {
                return errorSignalerNode.signalIndexOutOfRange(index, obj.length);
            }
            obj = (Object[]) obj[index];
        }
        var index = lookup[lookup.length - 1];
        if (index < 0 || index >= obj.length) {
            return errorSignalerNode.signalIndexOutOfRange(index, obj.length);
        }
        obj[index] = value;
        return value;
    }

    @Specialization
    Object doVector(Object value, LispVector vec, int[] lookup) {
        if (lookup.length != 1) {
            return signalWrongDimension(vec, 1, lookup.length);
        }
        var index = lookup[0];
        if (index < 0 || index >= vec.values().length) {
            return errorSignalerNode.signalIndexOutOfRange(index, vec.values().length);
        }
        vec.values()[index] = value;
        return value;
    }

    @Specialization
    Object doString(Object value, String str, int[] lookup) {
        return errorSignalerNode.signalDomainError(
            "Cannot change immutable value",
            str,
            ISLISPContext.get(this).lookupClass("<string>"));
    }

    @Specialization
    Object doMutableString(LispChar c, LispMutableString str, int[] lookup) {
        if (lookup.length != 1) {
            return signalWrongDimension(str, 1, lookup.length);
        }
        var index = lookup[0];
        str.chars()[index] = c;
        return c;
    }

    @Specialization(guards = "interop.hasArrayElements(o)", limit = "3")
    Object doTruffleVector(
        Object value,
        Object o,
        int[] lookup,
        @CachedLibrary("o") InteropLibrary interop
    ) {
        if (lookup.length != 1) {
            return signalWrongDimension(o, 1, lookup.length);
        }
        try {
            interop.writeArrayElement(o, lookup[0], value);
            return value;
        } catch (UnsupportedMessageException | UnsupportedTypeException | InvalidArrayIndexException e) {
            return errorSignalerNode.signalTruffleInteropError(e);
        }
    }

    @Fallback
    Object fallback(Object value, Object arr, int[] lookup) {
        var ctx = ISLISPContext.get(this);
        return errorSignalerNode.signalWrongType(arr, ctx.lookupClass("<basic-array>"));
    }

    @CompilerDirectives.TruffleBoundary
    Object signalWrongDimension(Object obj, int expected, int actual) {
        return errorSignalerNode.signalDomainError(
            "Expected dimension size " + expected + "; received " + actual,
            obj,
            ISLISPContext.get(this).lookupClass("<basic-array>"));
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPSetArefNodeGen.create(lang).getCallTarget());
    }
}
