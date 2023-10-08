package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispVector;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class ISLISPVector extends RootNode {

    protected ISLISPVector(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var values = new Object[frame.getArguments().length - 1];
        System.arraycopy(frame.getArguments(), 1, values, 0, values.length);
        return new LispVector(values);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPVector(lang).getCallTarget());
    }

}
