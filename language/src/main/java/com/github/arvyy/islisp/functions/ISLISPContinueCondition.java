package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPContinueException;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Implements `continue-condition` function, which defers condition handling to the next handler.
 */
public class ISLISPContinueCondition extends ISLISPBuiltinFunctionRootNode {

    public ISLISPContinueCondition(TruffleLanguage<?> language) {
        super("continue-condition", arg(1, 2), language);
    }

    @Override
    public Object executeImpl(VirtualFrame frame) {
        Object value;
        if (frame.getArguments().length == 2) {
            value = ISLISPContext.get(this).getNil();
        } else {
            value = frame.getArguments()[2];
        }
        var condition = frame.getArguments()[1];
        throw new ISLISPContinueException(condition, value);
    }

}
