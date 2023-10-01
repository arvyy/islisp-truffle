package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class BuiltinAdd extends RootNode {

    public BuiltinAdd(TruffleLanguage<?> language) {
        super(language);
    }

    abstract Object executeGeneric(Object a, Object b);

    @Override
    @ExplodeLoop
    public final Object execute(VirtualFrame frame) {
        Object sum = 0;
        for (int i = 1; i < frame.getArguments().length; i++) {
            sum = executeGeneric(sum, frame.getArguments()[i]);
        }
        return sum;
    }

    @Specialization
    Object executeInts(int a, int b) {
        return a + b;
    }

    @Fallback
    Object notNumbers(Object a, Object b) {
        throw new ISLISPError("Not numbers", this);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(BuiltinAddNodeGen.create(lang).getCallTarget());
    }

}
