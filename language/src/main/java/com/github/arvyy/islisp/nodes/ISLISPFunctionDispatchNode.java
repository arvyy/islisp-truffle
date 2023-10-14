package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;

public abstract class ISLISPFunctionDispatchNode extends Node {

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    public ISLISPFunctionDispatchNode() {
        errorSignalerNode = new ISLISPErrorSignalerNode();
    }

    public abstract Object executeDispatch(Object lispFunction, Object[] arguments);

    @ExplodeLoop
    @Specialization(guards = "function.callTarget() == callNode.getCallTarget()")
    public Object doDirect(
            LispFunction function,
            Object[] args,
            @Cached("create(function.callTarget())") DirectCallNode callNode) {
        var realArgs = new Object[args.length + 1];
        realArgs[0] = function.closure();
        System.arraycopy(args, 0, realArgs, 1, args.length);
        return callNode.call(realArgs);
    }

    @ExplodeLoop
    @Specialization(replaces = "doDirect")
    public Object doIndirect(
            LispFunction function,
            Object[] args,
            @Cached IndirectCallNode callNode) {
        var realArgs = new Object[args.length + 1];
        realArgs[0] = function.closure();
        System.arraycopy(args, 0, realArgs, 1, args.length);
        return callNode.call(function.callTarget(), realArgs);
    }

    @Fallback
    public Object notAFunction(Object notAFunction, Object[] args) {
        var ctx = ISLISPContext.get(this);
        var functionClass = ctx.lookupClass(ctx.namedSymbol("<function>").identityReference());
        return errorSignalerNode.signalWrongType(notAFunction, functionClass);
    }

}
