package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Pair;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class ISLISPSetCdr extends RootNode {

    protected ISLISPSetCdr(TruffleLanguage<?> language) {
        super(language);
    }

    protected abstract Object executeGeneric(Object arg1, Object arg2);

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            throw new ISLISPError("Wrong arg count", this);
        }
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    @Specialization
    public Object doPair(Pair p, Object v) {
        p.setCdr(v);
        return v;
    }

    @Fallback
    public Object fallback(Object arg1, Object arg2) {
        throw new ISLISPError("Not a pair", this);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPSetCdrNodeGen.create(lang).getCallTarget());
    }

}
