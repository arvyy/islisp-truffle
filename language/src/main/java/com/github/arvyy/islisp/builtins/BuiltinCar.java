package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Pair;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class BuiltinCar extends RootNode {

    protected BuiltinCar(TruffleLanguage<?> language) {
        super(language);
    }

    protected abstract Object executeGeneric(Object arg);

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            throw new ISLISPError("Wrong arg count", this);
        }
        return executeGeneric(frame.getArguments()[1]);
    }

    @Specialization
    public Object doPair(Pair p) {
        return p.car();
    }

    @Fallback
    public Object fallback(Object o) {
        throw new ISLISPError("Not a pair", this);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(BuiltinCarNodeGen.create(lang).getCallTarget());
    }

}
