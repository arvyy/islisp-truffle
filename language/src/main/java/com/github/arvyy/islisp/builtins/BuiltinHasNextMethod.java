package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Closure;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class BuiltinHasNextMethod extends RootNode {

    public BuiltinHasNextMethod(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var closure = (Closure) frame.getArguments()[0];
        var ctx = ISLISPContext.get(this);
        var applicables = closure.applicableMethods();
        return (applicables.aroundMethods().size() == 0 && applicables.primaryMethods().size() == 0)
                ? ctx.getNil()
                : ctx.getT();
    }

}
