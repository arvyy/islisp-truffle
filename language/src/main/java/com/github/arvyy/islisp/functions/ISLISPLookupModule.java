package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class ISLISPLookupModule extends RootNode {
    
    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPLookupModule(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length - 1,
                1,
                1);
        }
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
    
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPLookupModule(lang).getCallTarget());
    }
    
}
