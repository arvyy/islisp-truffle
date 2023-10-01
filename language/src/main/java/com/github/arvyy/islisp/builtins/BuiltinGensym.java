package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.SymbolReference;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class BuiltinGensym extends RootNode {

    protected BuiltinGensym(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        return new Symbol(makeGensymName(), new SymbolReference());
    }

    @CompilerDirectives.TruffleBoundary
    String makeGensymName() {
        return "gensym-" + ISLISPContext.get(this).gensymIndex();
    }


    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new BuiltinGensym(lang).getCallTarget());
    }

}
