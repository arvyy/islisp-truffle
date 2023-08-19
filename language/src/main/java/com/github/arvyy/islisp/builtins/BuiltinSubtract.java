package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispInteger;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class BuiltinSubtract extends RootNode {

    public BuiltinSubtract(TruffleLanguage<?> language) {
        super(language);
    }

    abstract Value executeGeneric(Value a, Value b);

    @Override
    @ExplodeLoop
    public final Value execute(VirtualFrame frame) {
        if (frame.getArguments().length == 2) {
            return executeGeneric(new LispInteger(0, null), (Value) frame.getArguments()[1]);
        }
        Value diff = (Value) frame.getArguments()[1];
        for (int i = 2; i < frame.getArguments().length; i++) {
            diff = executeGeneric(diff, (Value) frame.getArguments()[i]);
        }
        return diff;
    }

    @Specialization
    @ExplodeLoop
    Value executeInts(LispInteger a, LispInteger b) {
        return new LispInteger(a.value() - b.value(), null);
    }

    @Fallback
    Value notNumbers(Value a, Value b) {
        throw new ISLISPError("Not numbers", this);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(null, BuiltinSubtractNodeGen.create(lang).getCallTarget());
    }

}
