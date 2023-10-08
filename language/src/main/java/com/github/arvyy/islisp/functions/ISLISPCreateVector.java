package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispVector;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import java.util.Arrays;

public class ISLISPCreateVector extends RootNode {

    protected ISLISPCreateVector(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        Object el;
        if (frame.getArguments().length == 3) {
            el = frame.getArguments()[2];
        } else {
            el = ISLISPContext.get(this).getNil();
        }
        var values = new Object[(int) frame.getArguments()[1]];
        Arrays.fill(values, el);
        return new LispVector(values);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPCreateVector(lang).getCallTarget());
    }
}
