package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.runtime.LispBigInteger;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.math.BigInteger;

/**
 * Implements a `get-universal-time` function, unix timestamp in seconds.
 */
public class ISLISPGetUniversalTime extends RootNode {

    ISLISPGetUniversalTime(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var time = System.currentTimeMillis() / 1000;
        return new LispBigInteger(BigInteger.valueOf(time));
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPGetUniversalTime(lang).getCallTarget());
    }

}
