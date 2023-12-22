package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
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
    Object doInterop(
        Object o,
        Object[] args,
        @CachedLibrary(limit = "3") InteropLibrary interopLibrary
    ) {
        try {
            var value = interopLibrary.execute(o, args);
            if (value instanceof Boolean) {
                throw new RuntimeException("Bad");
            }
            return value;
        } catch (UnsupportedMessageException | UnsupportedTypeException | ArityException e) {
            //TODO
            throw new ISLISPError(e.getMessage(), this);
        }
    }

    @Fallback
    Object notAFunction(Object notAFunction, Object[] args) {
        var ctx = ISLISPContext.get(this);
        var functionClass = ctx.lookupClass(ctx.namedSymbol("<function>").identityReference());
        return errorSignalerNode.signalWrongType(notAFunction, functionClass);
    }

}
