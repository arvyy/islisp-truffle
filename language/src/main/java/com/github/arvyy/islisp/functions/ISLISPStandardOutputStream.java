package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispOutputStream;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class ISLISPStandardOutputStream extends RootNode {

    protected ISLISPStandardOutputStream(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        //TODO dynamic parameterization (with-standard-output ...)
        return new LispOutputStream(ISLISPContext.get(this).getEnv().out());
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPStandardOutputStream(lang).getCallTarget());
    }

}
