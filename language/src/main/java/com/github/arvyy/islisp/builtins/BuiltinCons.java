package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Pair;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class BuiltinCons extends RootNode {

    public BuiltinCons(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 3) {
            throw new ISLISPError("Wrong args", this);
        }
        return new Pair(frame.getArguments()[1], frame.getArguments()[2]);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new BuiltinCons(lang).getCallTarget());
    }
}
