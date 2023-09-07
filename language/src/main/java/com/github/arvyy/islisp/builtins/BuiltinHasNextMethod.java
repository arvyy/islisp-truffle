package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPGenericFunctionDispatchNode;
import com.github.arvyy.islisp.nodes.ISLISPGenericFunctionDispatchNodeGen;
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
        return closure.nextMethods().size() == 0? ctx.getNIL() : ctx.getT();
    }

}
