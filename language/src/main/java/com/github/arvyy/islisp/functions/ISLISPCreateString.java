package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispChar;
import com.github.arvyy.islisp.runtime.LispMutableString;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Implements `create-string` function.
 */
public abstract class ISLISPCreateString extends ISLISPBuiltinFunctionRootNode {

    /**
    * Construct node.
    * @param language truffle language reference
    */
    public ISLISPCreateString(TruffleLanguage<?> language) {
        super("create-string", arg(1, 2), language);
    }

    @Override
    public final Object executeImpl(VirtualFrame frame) {
        var count = frame.getArguments()[1];
        var character = frame.getArguments().length == 3
            ? frame.getArguments()[2]
            : new LispChar('?');
        return executeGeneric(count, character);
    }

    abstract Object executeGeneric(Object count, Object character);

    @Specialization
    Object doProper(int count, LispChar lispChar) {
        var datum = new LispChar[count];
        for (int i = 0; i < count; i++) {
            datum[i] = lispChar;
        }
        return new LispMutableString(datum);
    }

    @Fallback
    Object fallback(Object count, Object lispChar) {
        var ctx = ISLISPContext.get(this);
        if (lispChar instanceof  LispChar) {
            return errorSignalerNode.signalWrongType(count, ctx.lookupClass("<number>"));
        } else {
            return errorSignalerNode.signalWrongType(lispChar, ctx.lookupClass("<character>"));
        }
    }

}
