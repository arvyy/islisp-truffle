package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.nodes.ISLISPTypes;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements a `get-internal-real-time` function (and also aliased under `get-internal-run-time`),
 * unix timestamp in milliseconds.
 */
public class ISLISPGetInternalRealTime extends RootNode {

    ISLISPGetInternalRealTime(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var time = System.currentTimeMillis();
        return ISLISPTypes.longToBigInt(time);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPGetInternalRealTime(lang).getCallTarget());
    }

}
