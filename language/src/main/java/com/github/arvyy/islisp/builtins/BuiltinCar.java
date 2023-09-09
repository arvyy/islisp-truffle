package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPDefGenericExecutionNode;
import com.github.arvyy.islisp.nodes.ISLISPExpressionNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Pair;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

public abstract class BuiltinCar extends RootNode {

    protected BuiltinCar(TruffleLanguage<?> language) {
        super(language);
    }

    protected abstract Value executeGeneric(Object arg);

    @Override
    public final Value execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            throw new ISLISPError("Wrong arg count", this);
        }
        return executeGeneric(frame.getArguments()[1]);
    }

    @Specialization
    public Value doPair(Pair p) {
        return p.car();
    }

    @Fallback
    public Value fallback(Object o) {
        throw new ISLISPError("Not a pair", this);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(BuiltinCarNodeGen.create(lang).getCallTarget());
    }

}