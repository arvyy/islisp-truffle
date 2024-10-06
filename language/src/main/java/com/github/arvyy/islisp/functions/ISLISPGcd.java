package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.nodes.ISLISPTypes;
import com.github.arvyy.islisp.nodes.ISLISPTypesGen;
import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.LispBigInteger;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `gcd` function.
 */
@TypeSystemReference(ISLISPTypes.class)
public abstract class ISLISPGcd extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    protected ISLISPGcd(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length - 1,
                2,
                2);
        }
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    abstract Object executeGeneric(Object a, Object b);

    @Specialization
    int doInts(int a, int b) {
        if (b == 0) {
            return Math.abs(a);
        }
        return doInts(b, a % b);
    }

    @Specialization
    LispBigInteger doBigInts(LispBigInteger a, LispBigInteger b) {
        var value = a.data().gcd(b.data());
        return new LispBigInteger(value);
    }

    @Fallback
    Object fallback(Object a, Object b) {
        var ctx = ISLISPContext.get(this);
        var integerClass = ctx.lookupClass("<integer>");
        if (!ISLISPTypesGen.isImplicitLispBigInteger(a)) {
            return errorSignalerNode.signalWrongType(a, integerClass);
        }
        return errorSignalerNode.signalWrongType(b, integerClass);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        var callTarget = ISLISPGcdNodeGen.create(lang).getCallTarget();
        return new LispFunction(
            new Closure(null, null, null),
            callTarget,
            false,
            true);
    }

}
