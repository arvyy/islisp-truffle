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

/**
 * Implements various single-argument trig functions.
 */
@TypeSystemReference(ISLISPTypes.class)
@ReportPolymorphism
public abstract class ISLISPTrigFunctions extends RootNode {

    private final DoubleFunction impl;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPTrigFunctions(TruffleLanguage<?> language, DoubleFunction impl) {
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
    Object doDouble(double d) {
        return impl.compute(d);
    }

    @Specialization
    @CompilerDirectives.TruffleBoundary
    Object doBigInt(LispBigInteger b) {
        return impl.compute(b.data().doubleValue());
    }

    @Fallback
    Object fallback(Object o) {
        var ctx = ISLISPContext.get(this);
        return errorSignalerNode.signalWrongType(o, ctx.lookupClass("<number>"));
    }

    interface DoubleFunction {
        double compute(double arg);
    }

    static final class SinFunction implements ISLISPTrigFunctions.DoubleFunction {
        @Override
        public double compute(double arg) {
            return Math.sin(arg);
        }
    }

    static final class CosFunction implements ISLISPTrigFunctions.DoubleFunction {
        @Override
        public double compute(double arg) {
            return Math.cos(arg);
        }
    }

    static final class TanFunction implements ISLISPTrigFunctions.DoubleFunction {
        @Override
        public double compute(double arg) {
            return Math.tan(arg);
        }
    }

    /**
     * Construct LispFunction for sin function.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunctionSin(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPTrigFunctionsNodeGen.create(lang, new SinFunction()).getCallTarget());
    }

    /**
     * Construct LispFunction for cos function.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunctionCos(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPTrigFunctionsNodeGen.create(lang, new CosFunction()).getCallTarget());
    }

    /**
     * Construct LispFunction for cos function.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunctionTan(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPTrigFunctionsNodeGen.create(lang, new TanFunction()).getCallTarget());
    }

}
