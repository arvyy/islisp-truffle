package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Implements `lookup-module` function.
 */
public class ISLISPLookupModule extends ISLISPBuiltinFunctionRootNode {

    /**
    * Construct node.
    * @param language truffle language reference
    */
    public ISLISPLookupModule(TruffleLanguage<?> language) {
        super("lookup-module", arg(1), language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object executeImpl(VirtualFrame frame) {
        var arg = frame.getArguments()[1];
        var c = ISLISPContext.get(this);
        if (arg instanceof String name) {
            var m = c.getModule(name);
            if (m == null) {
                return c.getNil();
            } else {
                return m;
            }
        } else {
            return errorSignalerNode.signalWrongType(
                arg,
                c.lookupClass("string"));
        }
    }

}
