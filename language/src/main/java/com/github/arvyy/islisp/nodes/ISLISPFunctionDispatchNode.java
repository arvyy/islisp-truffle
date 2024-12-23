package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;

/**
 * Util node for dispatching a function call. Exists to reuse same specialization mechanism of Direct/Indirect calls
 * regardless if function being called is a global function or a funcall of a value.
 */
public abstract class ISLISPFunctionDispatchNode extends Node {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    /**
     * Create dispatch node.
     */
    public ISLISPFunctionDispatchNode() {
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    /**
     * Execute function dispatch.
     *
     * @param lispFunction function to be called
     * @param arguments function arguments
     * @return function result
     */
    public abstract Object executeDispatch(Object lispFunction, Object[] arguments);

    @Specialization(guards = {
        "interopLibrary.isExecutable(o)"
    }, limit = "3")
    Object doCached(
        Object o,
        Object[] args,
        @CachedLibrary("o") InteropLibrary interopLibrary
    ) {
        try {
            return interopLibrary.execute(o, args);
        } catch (UnsupportedMessageException | UnsupportedTypeException | ArityException e) {
            return errorSignalerNode.signalTruffleInteropError(e);
        }
    }

    @Fallback
    Object doUncached(Object o, Object[] args) {
        var interopLibrary = InteropLibrary.getUncached();
        if (interopLibrary.isExecutable(o)) {
            try {
                return interopLibrary.execute(o, args);
            } catch (UnsupportedMessageException | UnsupportedTypeException | ArityException e) {
                return errorSignalerNode.signalTruffleInteropError(e);
            }
        } else {
            var ctx = ISLISPContext.get(this);
            var functionClass = ctx.lookupClass("ROOT", ctx.namedSymbol("<function>"));
            return errorSignalerNode.signalWrongType(o, functionClass);
        }
    }

}
