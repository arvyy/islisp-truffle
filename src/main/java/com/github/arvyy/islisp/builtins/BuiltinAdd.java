package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispInteger;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.CallTarget;
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

    abstract Value executeGeneric(Value a, Value b);

    @Override
    @ExplodeLoop
    public final Value execute(VirtualFrame frame) {
        Value sum = new LispInteger(0);
        for (int i = 1; i < frame.getArguments().length; i++) {
            sum = executeGeneric(sum, (Value) frame.getArguments()[i]);
        }
        return sum;
    }

    @Specialization
    @ExplodeLoop
    Value executeInts(LispInteger a, LispInteger b) {
        return new LispInteger(a.value() + b.value());
    }

    @Fallback
    Value notNumbers(Value a, Value b) {
        throw new RuntimeException("Not numbers");
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(null, BuiltinAddNodeGen.create(lang).getCallTarget());
    }

}
