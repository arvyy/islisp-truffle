package com.github.arvyy.islisp.builtins;


import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;

public class BuiltinPrint extends RootNode {

    public BuiltinPrint(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Value execute(VirtualFrame frame) {
        println(frame.getArguments()[1]);
        return ISLISPContext.get(this).getNIL();
    }

    @CompilerDirectives.TruffleBoundary
    void println(Object value) {
        System.out.println(value);
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        var add = new BuiltinPrint(lang);
        return new LispFunction(add.getCallTarget());
    }

}
