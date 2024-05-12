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
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;

import java.math.BigDecimal;

/**
 * Implements numeric division function `quotient`.
 */
@TypeSystemReference(ISLISPTypes.class)
public abstract class ISLISPQuotient extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPQuotient(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    abstract Object executeGeneric(Object a, Object b);

    @Override
    @ExplodeLoop
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length < 2) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 2, -1);
        }
        Object result = frame.getArguments()[1];
        for (int i = 2; i < frame.getArguments().length; i++) {
            result = executeGeneric(result, frame.getArguments()[i]);
        }
        return result;
    }

    @Specialization
    Object doInts(int a, int b) {
        try {
            int result = a / b;
            if (a % b == 0) {
                return result;
            } else {
                return ((double) a) / b;
            }
        } catch (ArithmeticException e) {
            return handleArithmeticException(e, b == 0);
        }
    }

    @Specialization
    @CompilerDirectives.TruffleBoundary
    Object doBigInts(LispBigInteger a, LispBigInteger b) {
        try {
            var result = a.data().divideAndRemainder(b.data());
            if (result[1].signum() == 0) {
                return new LispBigInteger(result[0]);
            } else {
                return new BigDecimal(a.data()).divide(new BigDecimal(b.data())).doubleValue();
            }
        } catch (ArithmeticException e) {
            return handleArithmeticException(e, b.data().signum() == 0);
        }
    }

    @Specialization
    Object doDoubles(double a, double b) {
        if (b == 0) {
            return errorSignalerNode.signalDivisionByZero();
        }
        return a / b;
    }

    @CompilerDirectives.TruffleBoundary
    Object handleArithmeticException(ArithmeticException e, boolean divisorIsZero) {
        if (divisorIsZero) {
            return errorSignalerNode.signalDivisionByZero();
        } else {
            throw e;
        }
    }

    @Fallback
    Object notNumbers(Object a, Object b) {
        var ctx = ISLISPContext.get(this);
        var numberClass = ctx.lookupClass("<number>");
        return errorSignalerNode.signalWrongType(b, numberClass);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPQuotientNodeGen.create(lang).getCallTarget());
    }

}
