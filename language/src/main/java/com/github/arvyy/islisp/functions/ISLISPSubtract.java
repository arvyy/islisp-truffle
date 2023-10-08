package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class ISLISPSubtract extends RootNode {

    public ISLISPSubtract(TruffleLanguage<?> language) {
        super(language);
    }

    abstract Object executeGeneric(Object a, Object b);

    @Override
    @ExplodeLoop
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length == 2) {
            return executeGeneric(0, (Object) frame.getArguments()[1]);
        }
        Object diff = (Object) frame.getArguments()[1];
        for (int i = 2; i < frame.getArguments().length; i++) {
            diff = executeGeneric(diff, (Object) frame.getArguments()[i]);
        }
        return diff;
    }

    @Specialization
    @ExplodeLoop
    Object doInts(int a, int b) {
        return a - b;
    }

    @Fallback
    Object notNumbers(Object a, Object b) {
        throw new ISLISPError("Not numbers", this);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPSubtractNodeGen.create(lang).getCallTarget());
    }

}
