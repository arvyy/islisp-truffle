package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.nodes.ISLISPTypes;
import com.github.arvyy.islisp.runtime.LispBigInteger;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements sqrt (square root) function.
 */
//TODO negative number conditiion
@TypeSystemReference(ISLISPTypes.class)
public abstract class ISLISPSqrt extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPSqrt(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    abstract Object executeGeneric(Object value);

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 1, 1);
        }
        Object arg = frame.getArguments()[1];
        return executeGeneric(arg);
    }

    @Specialization
    Object doInt(int a) {
        if (a < 0) {
            return signalNegativeNumber(a);
        }
        var res = Math.sqrt(a);
        var intRes = (int) res;
        if (intRes == res) {
            return intRes;
        } else {
            return res;
        }
    }

    @Specialization
    Object doDouble(double a) {
        if (a < 0) {
            return signalNegativeNumber(a);
        }
        return Math.sqrt(a);
    }

    @Specialization
    @CompilerDirectives.TruffleBoundary
    Object doBigInt(LispBigInteger a) {
        try {
            return new LispBigInteger(a.data().sqrt());
        } catch (ArithmeticException ignored) {
            return signalNegativeNumber(a);
        }
    }

    Object signalNegativeNumber(Object o) {
        var ctx = ISLISPContext.get(this);
        var numberClass = ctx.lookupClass("<number>");
        return errorSignalerNode.signalDomainError("Argument to sqrt not a non-negative number", o, numberClass);
    }

    @Fallback
    Object notNumbers(Object v) {
        var ctx = ISLISPContext.get(this);
        var numberClass = ctx.lookupClass("<number>");
        return errorSignalerNode.signalWrongType(v, numberClass);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPSqrtNodeGen.create(lang).getCallTarget());
    }
}
