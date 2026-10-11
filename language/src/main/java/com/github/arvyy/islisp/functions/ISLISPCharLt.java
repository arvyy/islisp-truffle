package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispChar;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Implements `char<` function.
 */
public abstract class ISLISPCharLt extends ISLISPBuiltinFunctionRootNode {

    ISLISPCharLt(TruffleLanguage<?> language) {
        super("char<", arg(2), language);
    }

    @Override
    public final Object executeImpl(VirtualFrame frame) {
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    abstract Object executeGeneric(Object a, Object b);

    @Specialization
    Object doProper(LispChar a, LispChar b) {
        var ctx = ISLISPContext.get(this);
        return a.codepoint() < b.codepoint() ? ctx.getT() : ctx.getNil();
    }

    @Fallback
    Object fallback(Object a, Object b) {
        var ctx = ISLISPContext.get(this);
        var offender = (a instanceof LispChar) ? b : a;
        return errorSignalerNode.signalWrongType(offender, ctx.lookupClass("<character>"));
    }

}
