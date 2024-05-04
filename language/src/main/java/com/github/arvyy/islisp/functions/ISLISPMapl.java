package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Pair;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `mapl` function.
 */
public abstract class ISLISPMapl extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPMapl(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length < 3) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length - 1,
                2,
                -1);
        }
        var lists = new Object[frame.getArguments().length - 2];
        System.arraycopy(frame.getArguments(), 2, lists, 0, lists.length);
        try {
            return executeGeneric(frame.getArguments()[1], lists);
        } catch (InteropException e) {
            return errorSignalerNode.signalTruffleInteropError(e);
        }
    }

    abstract Object executeGeneric(Object mapper, Object[] lists) throws InteropException;

    @Specialization(limit = "3")
    Object doProper(
        Object o,
        Object[] lists,
        @CachedLibrary("o") InteropLibrary fn
    ) throws InteropException {
        Object[] args = new Object[lists.length];
        Object returnedList = lists[0];
        outter:
        while (true) {
            for (int i = 0; i < args.length; i++) {
                var arg = lists[i];
                if (Utils.isNil(arg)) {
                    break outter;
                } else if (arg instanceof Pair p) {
                    args[i] = p;
                    lists[i] = p.cdr();
                } else {
                    return errorSignalerNode.signalWrongType(arg, ISLISPContext.get(this).lookupClass("<list>"));
                }
            }
            fn.execute(o, args);
        }
        return returnedList;
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPMaplNodeGen.create(lang).getCallTarget());
    }

}
