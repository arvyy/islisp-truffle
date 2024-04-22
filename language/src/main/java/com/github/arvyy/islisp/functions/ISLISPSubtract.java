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
 * Implements numeric subtraction function `-`.
 */
@TypeSystemReference(ISLISPTypes.class)
public abstract class ISLISPSubtract extends RootNode {


    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPSubtract(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
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
    LispBigInteger doBigInts(LispBigInteger a, LispBigInteger b) {
        return new LispBigInteger(a.data().subtract(b.data()));
    }

    @Specialization
    double doDoubles(double a, double b) {
        return a - b;
    }

    @Fallback
    Object notNumbers(Object a, Object b) {
        var ctx = ISLISPContext.get(this);
        var numberClass = ctx.lookupClass("<number>");
        return errorSignalerNode.signalWrongType(b, numberClass);
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
