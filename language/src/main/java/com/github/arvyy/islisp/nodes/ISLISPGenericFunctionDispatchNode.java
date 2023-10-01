package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.ArraySlice;
import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.GenericMethodApplicableMethods;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;

public abstract class ISLISPGenericFunctionDispatchNode extends Node {

    public abstract Object executeDispatch(GenericMethodApplicableMethods methods, Object[] arguments);

    @ExplodeLoop
    @Specialization
    public Object doIndirect(
            GenericMethodApplicableMethods applicableMethods,
            Object[] args,
            @Cached IndirectCallNode callNode) {
        var realArgs = new Object[args.length + 1];
        System.arraycopy(args, 0, realArgs, 1, args.length);
        if (applicableMethods.aroundMethods().size() != 0) {
            var newApplicableMethods = new GenericMethodApplicableMethods(
                    applicableMethods.primaryMethods(),
                    applicableMethods.aroundMethods().drop(1),
                    applicableMethods.beforeMethods(),
                    applicableMethods.afterMethods()
            );
            realArgs[0] = new Closure(null, newApplicableMethods, args);
            return callNode.call(applicableMethods.aroundMethods().get(0), realArgs);
        } else {
            for (int i = applicableMethods.beforeMethods().start(); i < applicableMethods.beforeMethods().end(); i++) {
                callNode.call(applicableMethods.beforeMethods().els()[i], realArgs);
            }
            var newApplicableMethods = new GenericMethodApplicableMethods(
                    applicableMethods.primaryMethods().drop(1),
                    new ArraySlice<>(new CallTarget[0]),
                    new ArraySlice<>(new CallTarget[0]),
                    new ArraySlice<>(new CallTarget[0])
            );
            realArgs[0] = new Closure(null, newApplicableMethods, args);
            //TODO handle when primary methods empty?
            var result = callNode.call(applicableMethods.primaryMethods().get(0), realArgs);
            realArgs[0] = null;
            for (int i = applicableMethods.afterMethods().start(); i < applicableMethods.afterMethods().end(); i++) {
                callNode.call(applicableMethods.afterMethods().els()[i], realArgs);
            }
            return result;
        }
    }

}
