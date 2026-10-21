package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Pair;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Implements `car` function, returning pair's first slot.
 */
public abstract class ISLISPCar extends ISLISPBuiltinFunctionRootNode {

    ISLISPCar(TruffleLanguage<?> language) {
        super("car", arg(1, 1), language);
    }

    abstract Object executeGeneric(Object arg);

    @Override
    public final Object executeImpl(VirtualFrame frame) {
        return executeGeneric(frame.getArguments()[1]);
    }

    @Specialization
    Object doPair(Pair p) {
        return p.car();
    }

    @Fallback
    Object fallback(Object o) {
        return errorSignalerNode.signalWrongType(o, ISLISPContext.get(this).lookupClass("<cons>"));
    }

}
