package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPGenericFunctionDispatchNode;
import com.github.arvyy.islisp.nodes.ISLISPGenericFunctionDispatchNodeGen;
import com.github.arvyy.islisp.runtime.Closure;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `call-next-method`, invoking next applicable generic method in chain.
 */
public class ISLISPCallNextMethod extends RootNode {

    @Child
    private ISLISPGenericFunctionDispatchNode dispatchNode;

    /**
     * Create call-next-method node.
     * @param language language reference
     */
    public ISLISPCallNextMethod(TruffleLanguage<?> language) {
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
