package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPGenericFunctionDispatchNode;
import com.github.arvyy.islisp.nodes.ISLISPGenericFunctionDispatchNodeGen;
import com.github.arvyy.islisp.runtime.Closure;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class BuiltinCallNextMethod extends RootNode {

    @Child
    private final ISLISPGenericFunctionDispatchNode dispatchNode;

    public BuiltinCallNextMethod(TruffleLanguage<?> language) {
        super(language);
        dispatchNode = ISLISPGenericFunctionDispatchNodeGen.create();
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var closure = (Closure) frame.getArguments()[0];
        var applicables = closure.applicableMethods();
        if (applicables.aroundMethods().size() == 0 && applicables.primaryMethods().size() == 0) {
            throw new ISLISPError("No next method", this);
        }
        return dispatchNode.executeDispatch(applicables, closure.args());
    }

}
