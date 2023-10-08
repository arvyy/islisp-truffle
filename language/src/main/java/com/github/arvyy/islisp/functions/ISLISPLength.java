package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispVector;
import com.github.arvyy.islisp.runtime.Pair;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class ISLISPLength extends RootNode {

    protected ISLISPLength(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        return executeGeneric(frame.getArguments()[1]);
    }

    abstract Object executeGeneric(Object obj);

    @Specialization
    Object doVector(LispVector vector) {
        return vector.values().length;
    }

    @Specialization
    Object doString(String str) {
        return str.length();
    }

    @Specialization
    Object doSymbol(Symbol s) {
        if (s.identityReference() == ISLISPContext.get(this).getNil().identityReference()) {
            return 0;
        } else {
            throw new ISLISPError("Not a sequence", this);
        }
    }

    @Specialization
    Object doPair(Pair p) {
        Object v = p;
        int len = 0;
        while (v instanceof Pair pair) {
            len++;
            v = pair.cdr();
        }
        return len;
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPLengthNodeGen.create(lang).getCallTarget());
    }

}
