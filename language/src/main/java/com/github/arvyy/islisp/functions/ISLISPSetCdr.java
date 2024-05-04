package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Pair;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `set-cdr` function, assigns value to the second slot of a pair.
 */
public abstract class ISLISPSetCdr extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPSetCdr(TruffleLanguage<?> language) {
        super(language);
    }

    abstract Object executeGeneric(Object arg1, Object arg2);

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 2, 2);
        }
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    @Specialization
    Object doPair(Object v, Pair p) {
        p.setCdr(v);
        return v;
    }

    @Fallback
    Object fallback(Object arg1, Object arg2) {
        throw new ISLISPError("Not a pair", this);
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPSetCdrNodeGen.create(lang).getCallTarget());
    }

}
