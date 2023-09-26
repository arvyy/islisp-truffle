package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Pair;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class BuiltinSetCar extends RootNode {

    protected BuiltinSetCar(TruffleLanguage<?> language) {
        super(language);
    }

    protected abstract Value executeGeneric(Object arg1, Object arg2);

    @Override
    public final Value execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            throw new ISLISPError("Wrong arg count", this);
        }
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    @Specialization
    public Value doPair(Pair p, Value v) {
        p.setCar(v);
        return v;
    }

    @Fallback
    public Value fallback(Object arg1, Object arg2) {
        throw new ISLISPError("Not a pair", this);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(BuiltinSetCarNodeGen.create(lang).getCallTarget());
    }

}
