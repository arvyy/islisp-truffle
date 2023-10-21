package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispVector;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.util.Arrays;

/**
 * Implements `create-vector` function; creates a vector instance.
 */
public class ISLISPCreateVector extends RootNode {

    ISLISPCreateVector(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        Object el;
        if (frame.getArguments().length == 3) {
            el = frame.getArguments()[2];
        } else {
            el = ISLISPContext.get(this).getNil();
        }
        var values = new Object[(int) frame.getArguments()[1]];
        Arrays.fill(values, el);
        return new LispVector(values);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPCreateVector(lang).getCallTarget());
    }
}
