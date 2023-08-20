package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class BuiltinEq extends RootNode {

    protected BuiltinEq(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (frame.getArguments()[1] == frame.getArguments()[2]) {
            return ISLISPContext.get(null).getT();
        } else {
            return ISLISPContext.get(null).getNIL();
        }
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(null, new BuiltinEq(lang).getCallTarget());
    }

}
