package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispInteger;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class BuiltinNumericGt extends RootNode {

    protected BuiltinNumericGt(TruffleLanguage<?> language) {
        super(language);
    }

    abstract Value executeGeneric(Value a, Value b);

    @Override
    public final Object execute(VirtualFrame frame) {
        return executeGeneric((Value) frame.getArguments()[1], (Value) frame.getArguments()[2]);
    }

    @Specialization
    Value executeInts(LispInteger a, LispInteger b) {
        if (a.value() > b.value())
            return new Symbol("T");
        return Symbol.NIL;
    }

    @Fallback
    Value notNumbers(Value a, Value b) {
        throw new RuntimeException("Not numbers");
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(null, BuiltinNumericGtNodeGen.create(lang).getCallTarget());
    }

}
