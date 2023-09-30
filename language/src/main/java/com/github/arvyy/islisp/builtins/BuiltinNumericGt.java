package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispInteger;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.profiles.CountingConditionProfile;

public abstract class BuiltinNumericGt extends RootNode {

    private final CountingConditionProfile profile;

    protected BuiltinNumericGt(TruffleLanguage<?> language) {
        super(language);
        profile = CountingConditionProfile.create();
    }

    abstract Value executeGeneric(Value a, Value b);

    @Override
    public final Object execute(VirtualFrame frame) {
        return executeGeneric((Value) frame.getArguments()[1], (Value) frame.getArguments()[2]);
    }

    @Specialization
    Value executeInts(LispInteger a, LispInteger b) {
        if (profile.profile(a.value() > b.value())) {
            return ISLISPContext.get(this).getT();
        }
        return ISLISPContext.get(this).getNil();
    }

    @Fallback
    Value notNumbers(Value a, Value b) {
        throw new ISLISPError("Not numbers", this);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(BuiltinNumericGtNodeGen.create(lang).getCallTarget());
    }

}
