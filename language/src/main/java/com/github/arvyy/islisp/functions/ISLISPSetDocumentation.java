package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Implements `set-documentation` function.
 */
public class ISLISPSetDocumentation extends ISLISPBuiltinFunctionRootNode {

    /**
    * Construct node.
    * @param language truffle language reference
    */
    public ISLISPSetDocumentation(TruffleLanguage<?> language) {
        super("set-documentation", arg(3), language);
    }

    @Override
    public Object executeImpl(VirtualFrame frame) {
        var args = frame.getArguments();
        var ctx = ISLISPContext.get(this);
        var doc = args[1];
        if (args[2] instanceof Symbol ns) {
            if (args[3] instanceof Symbol s) {
                return ctx.getCurrentModule().setDocumentation(ns, s, doc);
            } else {
                return errorSignalerNode.signalWrongType(args[3], ctx.lookupClass("<symbol>"));
            }
        } else {
            return errorSignalerNode.signalWrongType(args[2], ctx.lookupClass("<symbol>"));
        }
    }

}
