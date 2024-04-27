package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispChar;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispMutableString;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `create-string` function.
 */
public abstract class ISLISPCreateString extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPCreateString(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length - 1, 2, 2);
        }
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    abstract Object executeGeneric(Object count, Object character);

    @Specialization
    Object doProper(int count, LispChar lispChar) {
        var datum = new LispChar[count];
        for (int i = 0; i < count; i++) {
            datum[i] = lispChar;
        }
        return new LispMutableString(datum);
    }

    @Fallback
    Object fallback(Object count, Object lispChar) {
        var ctx = ISLISPContext.get(this);
        if (lispChar instanceof  LispChar) {
            return errorSignalerNode.signalWrongType(count, ctx.lookupClass("<number>"));
        } else {
            return errorSignalerNode.signalWrongType(lispChar, ctx.lookupClass("<character>"));
        }
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPCreateStringNodeGen.create(lang).getCallTarget());
    }

}
