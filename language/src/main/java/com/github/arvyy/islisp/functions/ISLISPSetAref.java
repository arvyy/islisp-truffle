package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispArray;
import com.github.arvyy.islisp.runtime.LispBigInteger;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispVector;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
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
            if (lookup[i] < 0) {
                //TODO
                throw new ISLISPError("Negative array index", this);
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
    Object executeArray(Object value, LispArray arr, int[] lookup) {
        if (arr.dimensions() != lookup.length) {
            //TODO
            throw new ISLISPError("Wrong dimension count", this);
        }
        Object[] obj = arr.data();
        for (var i = 0; i < lookup.length - 1; i++) {
            obj = (Object[]) obj[lookup[i]];
        }
        obj[lookup[lookup.length - 1]] = value;
        return value;
    }

    @Specialization
    Object executeVector(Object value, LispVector vec, int[] lookup) {
        if (lookup.length != 1) {
            //TODO
            throw new ISLISPError("Wrong dimension count", this);
        }
        vec.values()[lookup[0]] = value;
        return value;
    }

    //TODO string

    @Fallback
    Object fallback(Object value, Object arr, int[] lookup) {
        var ctx = ISLISPContext.get(this);
        return errorSignalerNode.signalWrongType(arr, ctx.lookupClass("<basic-array>"));
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
