package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Pair;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class ISLISPCar extends RootNode {

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    protected ISLISPCar(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode();
    }

    protected abstract Object executeGeneric(Object arg);

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 1, 1);
        }
        return executeGeneric(frame.getArguments()[1]);
    }

    @Specialization
    public Object doPair(Pair p) {
        return p.car();
    }

    @Fallback
    public Object fallback(Object o) {
        return errorSignalerNode.signalWrongType(o, ISLISPContext.get(this).lookupClass("<cons>"));
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPCarNodeGen.create(lang).getCallTarget());
    }

}
