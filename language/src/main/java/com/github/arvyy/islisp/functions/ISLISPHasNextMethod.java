package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Closure;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `next-method-p`, predicate informing if there are more applicable methods in the chain
 * and thus if a `call-next-method` is valid.
 */
public class ISLISPHasNextMethod extends RootNode {

    /**
     * Create next-method-p node.
     * @param language language reference
     */
    public ISLISPHasNextMethod(TruffleLanguage<?> language) {
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
