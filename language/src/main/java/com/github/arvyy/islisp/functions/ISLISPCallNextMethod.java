package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.nodes.ISLISPGenericFunctionDispatchNode;
import com.github.arvyy.islisp.nodes.ISLISPGenericFunctionDispatchNodeGen;
import com.github.arvyy.islisp.runtime.Closure;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Implements `call-next-method`, invoking next applicable generic method in chain.
 */
public class ISLISPCallNextMethod extends ISLISPBuiltinFunctionRootNode {

    @Child
    ISLISPGenericFunctionDispatchNode dispatchNode;

    /**
     * Create call-next-method node.
     * @param language language reference
     */
    public ISLISPCallNextMethod(TruffleLanguage<?> language) {
        super("call-next-method", vararg(0), language);
        dispatchNode = ISLISPGenericFunctionDispatchNodeGen.create();
    }

    @Override
    public Object executeImpl(VirtualFrame frame) {
        var closure = (Closure) frame.getArguments()[0];
        var applicables = closure.applicableMethods();
        if (applicables.aroundMethods().size() == 0 && applicables.primaryMethods().size() == 0) {
            return errorSignalerNode.signalNoNextMethod();
        }
        return dispatchNode.executeDispatch(applicables, closure.args());
    }

    @Override
    public boolean isCloningAllowed() {
        return true;
    }
}
