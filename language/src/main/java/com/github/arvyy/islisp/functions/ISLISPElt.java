package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispChar;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispVector;
import com.github.arvyy.islisp.runtime.Pair;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class ISLISPElt extends RootNode {

    protected ISLISPElt(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    abstract Object executeGeneric(Object seq, Object index);

    @Specialization
    Object doList(Pair p, int index) {
        var value = p;
        for (int i = 0; i < index; i++) {
            value = (Pair) value.cdr();
        }
        return value.car();
    }

    @Specialization
    Object doVector(LispVector vec, int index) {
        return vec.values()[index];
    }

    @Specialization
    Object doString(String str, int index) {
        return new LispChar(str.codePointAt(index));
    }

    @Fallback
    Object fallback(Object seq, Object index) {
        throw new ISLISPError("Bad sequence or index", this);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPEltNodeGen.create(lang).getCallTarget());
    }

}
