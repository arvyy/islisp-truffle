package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;

import java.math.BigInteger;

/**
 * Implements numeric subtraction function `-`.
 */
public abstract class ISLISPSubtract extends RootNode {

    ISLISPSubtract(TruffleLanguage<?> language) {
        super(language);
    }

    abstract Object executeGeneric(Object a, Object b);

    @Override
    @ExplodeLoop
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length == 2) {
            return executeGeneric(0, (Object) frame.getArguments()[1]);
        }
        Number diff = (Number) frame.getArguments()[1];
        for (int i = 2; i < frame.getArguments().length; i++) {
            diff = (Number) executeGeneric(diff, (Number) frame.getArguments()[i]);
        }
        return diff;
    }

    @Specialization(rewriteOn = ArithmeticException.class)
    int doInts(int a, int b) {
        return Math.subtractExact(a, b);
    }

    @Specialization
    @CompilerDirectives.TruffleBoundary
    BigInteger doBigInts(BigInteger a, BigInteger b) {
        return a.subtract(b);
    }

    @Specialization
    double doDoubles(double a, double b) {
        return a - b;
    }

    @Fallback
    Object notNumbers(Object a, Object b) {
        throw new ISLISPError("Not numbers", this);
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPSubtractNodeGen.create(lang).getCallTarget());
    }

}
