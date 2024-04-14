package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.nodes.ISLISPTypes;
import com.github.arvyy.islisp.runtime.LispBigInteger;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.math.BigDecimal;

/**
 * Implements various single-argument rounding functions.
 */
@TypeSystemReference(ISLISPTypes.class)
@ReportPolymorphism
public abstract class ISLISPRoundingFunctions extends RootNode {

    private final RoundingFunction impl;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPRoundingFunctions(TruffleLanguage<?> language, RoundingFunction impl) {
        super(language);
        this.impl = impl;
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        var args = frame.getArguments();
        if (args.length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(args.length - 1, 1, 1);
        }
        return executeGeneric(args[1]);
    }

    abstract Object executeGeneric(Object arg);

    @Specialization
    Object doInt(int i) {
        return i;
    }

    @Specialization
    Object doDouble(double d) {
        var r = impl.compute(d);
        if (r > Integer.MAX_VALUE || r < Integer.MIN_VALUE) {
            return asLispBigInteger(r);
        } else {
            return (int) r;
        }
    }

    @CompilerDirectives.TruffleBoundary
    LispBigInteger asLispBigInteger(double r) {
        var v = BigDecimal.valueOf(r).toBigInteger();
        return new LispBigInteger(v);
    }

    @Specialization
    Object doBigInt(LispBigInteger b) {
        return b;
    }

    @Fallback
    Object fallback(Object o) {
        var ctx = ISLISPContext.get(this);
        return errorSignalerNode.signalWrongType(o, ctx.lookupClass("<number>"));
    }

    interface RoundingFunction {
        double compute(double arg);
    }

    static final class CeilingFunction implements ISLISPRoundingFunctions.RoundingFunction {
        @Override
        public double compute(double arg) {
            return Math.ceil(arg);
        }
    }

    static final class FloorFunction implements ISLISPRoundingFunctions.RoundingFunction {
        @Override
        public double compute(double arg) {
            return Math.floor(arg);
        }
    }

    static final class TruncateFunction implements ISLISPRoundingFunctions.RoundingFunction {
        @Override
        public double compute(double arg) {
            return arg < 0 ? Math.ceil(arg) : Math.floor(arg);
        }
    }

    static final class RoundFunction implements ISLISPRoundingFunctions.RoundingFunction {
        @Override
        public double compute(double arg) {
            double res = Math.round(arg);
            // if number is exactly at midpoint, it must round to an even side
            if (Math.abs(arg % 1) == (1d / 2) && Math.abs(res % 2) == 1) {
                res -= 1;
            }
            return res;
        }
    }

    /**
     * Construct LispFunction for ceiling function.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunctionCeiling(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPRoundingFunctionsNodeGen.create(lang, new CeilingFunction()).getCallTarget());
    }

    /**
     * Construct LispFunction for floor function.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunctionFloor(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPRoundingFunctionsNodeGen.create(lang, new FloorFunction()).getCallTarget());
    }

    /**
     * Construct LispFunction for truncate function.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunctionTruncate(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPRoundingFunctionsNodeGen.create(lang, new TruncateFunction()).getCallTarget());
    }

    /**
     * Construct LispFunction for round function.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunctionRound(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPRoundingFunctionsNodeGen.create(lang, new RoundFunction()).getCallTarget());
    }

}
