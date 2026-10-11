package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.runtime.Pair;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Implements `cons` function, creates a new pair from given two arguments.
 */
public class ISLISPCons extends ISLISPBuiltinFunctionRootNode {

    /**
    * Construct node.
    * @param language truffle language reference
    */
    public ISLISPCons(TruffleLanguage<?> language) {
        super("cons", arg(2), language);
    }

    @Override
    public Object executeImpl(VirtualFrame frame) {
        return new Pair(frame.getArguments()[1], frame.getArguments()[2]);
    }

}
