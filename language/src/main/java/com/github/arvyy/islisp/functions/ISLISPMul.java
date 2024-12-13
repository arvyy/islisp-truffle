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

/**
 * Implements numeric multiplication function `*`.
 */
@TypeSystemReference(ISLISPTypes.class)
public abstract class ISLISPMul extends RootNode {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPMul(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    abstract Object executeGeneric(Object a, Object b);

    @Override
    @ExplodeLoop
    public final Object execute(VirtualFrame frame) {
        Object product = 1;
        for (int i = 1; i < frame.getArguments().length; i++) {
            product = executeGeneric(product, frame.getArguments()[i]);
        }
        return product;
    }

    @Specialization(rewriteOn = ArithmeticException.class)
    int doInts(int a, int b) {
        return Math.multiplyExact(a, b);
    }

    @Specialization
    @CompilerDirectives.TruffleBoundary
    LispBigInteger doBigInts(LispBigInteger a, LispBigInteger b) {
        return a.multiply(b);
    }

    @Specialization
    double doDoubles(double a, double b) {
        return a * b;
    }

    @Fallback
    Object notNumbers(Object a, Object b) {
        var ctx = ISLISPContext.get(this);
        var numberClass = ctx.lookupClass(ctx.namedSymbol("<number>"));
        return errorSignalerNode.signalWrongType(b, numberClass);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPMulNodeGen.create(lang).getCallTarget());
    }

}
