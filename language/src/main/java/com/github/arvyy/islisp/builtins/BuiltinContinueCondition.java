package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPContinueException;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class BuiltinContinueCondition extends RootNode {

    protected BuiltinContinueCondition(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        Object value;
        if (frame.getArguments().length == 2) {
            value = ISLISPContext.get(this).getNil();
        } else {
            value = frame.getArguments()[2];
        }
        var condition = frame.getArguments()[1];
        throw new ISLISPContinueException(condition, value);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new BuiltinContinueCondition(lang).getCallTarget());
    }
}
