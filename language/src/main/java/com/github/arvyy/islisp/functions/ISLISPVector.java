package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispVector;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `vector` function.
 */
public class ISLISPVector extends RootNode {

    ISLISPVector(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var values = new Object[frame.getArguments().length - 1];
        System.arraycopy(frame.getArguments(), 1, values, 0, values.length);
        return new LispVector(values);
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPVector(lang).getCallTarget());
    }

}
