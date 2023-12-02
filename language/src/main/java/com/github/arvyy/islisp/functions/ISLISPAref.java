package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispArray;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispVector;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.math.BigInteger;

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
            } else if (arg instanceof BigInteger) {
                lookup[i] = bigintValue((BigInteger) arg);
            } else {
                var ctx = ISLISPContext.get(this);
                return errorSignalerNode.signalWrongType(arg, ctx.lookupClass("<integer>"));
            }
            if (lookup[i] < 0) {
                //TODO
                throw new ISLISPError("Negative array index", this);
            }
        }
        return executeGeneric(frame.getArguments()[1], lookup);
    }

    @CompilerDirectives.TruffleBoundary
    int bigintValue(BigInteger arg) {
        return arg.intValueExact();
    }

    abstract Object executeGeneric(Object array, int[] lookup);

    @Specialization
    Object executeArray(LispArray arr, int[] lookup) {
        if (arr.dimensions() != lookup.length) {
            //TODO
            throw new ISLISPError("Wrong dimension count", this);
        }
        Object obj = arr.data();
        for (var i: lookup) {
            obj = ((Object[]) obj)[i];
        }
        return obj;
    }

    @Specialization
    Object executeVector(LispVector vec, int[] lookup) {
        if (lookup.length != 1) {
            //TODO
            throw new ISLISPError("Wrong dimension count", this);
        }
        return vec.values()[lookup[0]];
    }

    //TODO string

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
