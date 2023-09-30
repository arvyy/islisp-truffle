package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;

public abstract class ISLISPFunctionDispatchNode extends Node {

    public abstract Value executeDispatch(Object lispFunction, Object[] arguments);

    @ExplodeLoop
    @Specialization(guards = "function.callTarget() == callNode.getCallTarget()")
    public Value executeDirect(
            LispFunction function,
            Object[] args,
            @Cached("create(function.callTarget())") DirectCallNode callNode) {
        var realArgs = new Object[args.length + 1];
        realArgs[0] = function.closure();
        System.arraycopy(args, 0, realArgs, 1, args.length);
        return (Value) callNode.call(realArgs);
    }

    @ExplodeLoop
    @Specialization(replaces = "executeDirect")
    public Value executeIndirect(
            LispFunction function,
            Object[] args,
            @Cached IndirectCallNode callNode) {
        var realArgs = new Object[args.length + 1];
        realArgs[0] = function.closure();
        System.arraycopy(args, 0, realArgs, 1, args.length);
        return (Value) callNode.call(function.callTarget(), realArgs);
    }

    @Fallback
    public Value notAFunction(Object notAFunction, Object[] args) {
        throw new ISLISPError("Not a function", this);
    }
}
