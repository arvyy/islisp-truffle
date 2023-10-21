package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements eq function, reference equality comparator.
 */
public class ISLISPEq extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPEq(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode();
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 2, 2);
        }
        if (isEq(frame.getArguments()[1], frame.getArguments()[2])) {
            return ISLISPContext.get(null).getT();
        } else {
            return ISLISPContext.get(null).getNil();
        }
    }

    boolean isEq(Object o1, Object o2) {
        if (o1 instanceof Symbol s1 && o2 instanceof Symbol s2) {
            return s1.identityReference().getId() == s2.identityReference().getId();
        }
        return o1 == o2; // TODO
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPEq(lang).getCallTarget());
    }

}
