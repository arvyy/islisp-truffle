package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.runtime.ArraySlice;
import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;

import java.util.List;

public abstract class ISLISPGenericFunctionDispatchNode extends Node {

    public abstract Value executeDispatch(ArraySlice<CallTarget> applicableMethods, Object[] arguments);

    @ExplodeLoop
    @Specialization
    public Value executeIndirect(
            ArraySlice<CallTarget> applicableMethods,
            Object[] args,
            @Cached IndirectCallNode callNode) {
        var realArgs = new Object[args.length + 1];
        realArgs[0] = new Closure(null, applicableMethods.drop(1), args);
        System.arraycopy(args, 0, realArgs, 1, args.length);
        return (Value) callNode.call(applicableMethods.get(0), realArgs);
    }

}