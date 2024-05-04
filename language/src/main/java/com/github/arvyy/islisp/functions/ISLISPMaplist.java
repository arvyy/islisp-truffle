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
 * Implements `maplist` function.
 */
public abstract class ISLISPMaplist extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPMaplist(TruffleLanguage<?> language) {
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
        var nil = ISLISPContext.get(this).getNil();
        Object[] args = new Object[lists.length];
        Pair head = null;
        Pair tail = null;
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
            var mappedValue = fn.execute(o, args);
            if (head == null) {
                head = new Pair(mappedValue, nil);
                tail = head;
            } else {
                var newTail = new Pair(mappedValue, nil);
                tail.setCdr(newTail);
                tail = newTail;
            }
        }
        if (head == null) {
            return nil;
        }
        return head;
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPMaplistNodeGen.create(lang).getCallTarget());
    }

}
