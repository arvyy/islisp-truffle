package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class BuiltinEq extends RootNode {

    protected BuiltinEq(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (isEq(frame.getArguments()[1], frame.getArguments()[2])) {
            return ISLISPContext.get(null).getT();
        } else {
            return ISLISPContext.get(null).getNil();
        }
    }

    public static boolean isEq(Object o1, Object o2) {
        if (o1 instanceof Symbol s1 && o2 instanceof Symbol s2) {
            return s1.identityReference().getId() == s2.identityReference().getId();
        }
        return o1 == o2; // TODO
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new BuiltinEq(lang).getCallTarget());
    }

}
