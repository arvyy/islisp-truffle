package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.nodes.ISLISPTypes;
import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.LispBigInteger;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.math.BigInteger;

/**
 * Implements `isqrt` function.
 */
@TypeSystemReference(ISLISPTypes.class)
public abstract class ISLISPIsqrt extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    protected ISLISPIsqrt(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length - 1,
                1,
                1);
        }
        return executeGeneric(frame.getArguments()[1]);
    }

    abstract Object executeGeneric(Object a);

    //TODO int specialization

    @Specialization
    @CompilerDirectives.TruffleBoundary
    Object doBigInt(LispBigInteger a) {
        if (a.data().compareTo(BigInteger.ZERO) < 0) {
            var ctx = ISLISPContext.get(this);
            var integerClass = ctx.lookupClass("<integer>");
            return errorSignalerNode.signalDomainError("negative value passed to isqrt", a, integerClass);
        }
        var value = a.data().sqrt();
        if (value.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
            return new LispBigInteger(value);
        } else {
            return value.intValue();
        }
    }

    @Fallback
    Object fallback(Object a) {
        var ctx = ISLISPContext.get(this);
        var integerClass = ctx.lookupClass("<integer>");
        return errorSignalerNode.signalWrongType(a, integerClass);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        var callTarget = ISLISPIsqrtNodeGen.create(lang).getCallTarget();
        return new LispFunction(
            new Closure(null, null, null),
            callTarget,
            false,
            true);
    }

}
