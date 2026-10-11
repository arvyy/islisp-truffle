package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Implements `documentation` function.
 */
public class ISLISPDocumentation extends ISLISPBuiltinFunctionRootNode {

    /**
    * Construct node.
    * @param language truffle language reference
    */
    public ISLISPDocumentation(TruffleLanguage<?> language) {
        super("documentation", arg(1, 2), language);
    }

    @Override
    public final Object executeImpl(VirtualFrame frame) {
        var args = frame.getArguments();
        var ctx = ISLISPContext.get(this);
        if (args.length == 2) {
            if (args[1] instanceof Symbol s) {
                return ctx.getCurrentModule().getDocumentationIncludeImports(s);
            } else {
                return errorSignalerNode.signalWrongType(args[1], ctx.lookupClass("<symbol>"));
            }
        } else {
            if (args[1] instanceof Symbol ns) {
                if (args[2] instanceof Symbol s) {
                    return ctx.getCurrentModule().getDocumentationIncludeImports(ns, s);
                } else {
                    return errorSignalerNode.signalWrongType(args[2], ctx.lookupClass("<symbol>"));
                }
            } else {
                return errorSignalerNode.signalWrongType(args[1], ctx.lookupClass("<symbol>"));
            }
        }
    }

}
