package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.ArraySlice;
import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.GenericMethodApplicableMethods;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;

/**
 * Helper node for dispatching generic calls. Invoked on initial generic call and subsequent `call-next-method` calls.
 */
public abstract class ISLISPGenericFunctionDispatchNode extends Node {

    /**
     * Execute dispatch of the generic function.
     *
     * @param methods collected applicable methods for this specific invocation
     * @param arguments function arguments
     * @return generic function result
     */
    public abstract Object executeDispatch(GenericMethodApplicableMethods methods, Object[] arguments);

    @ExplodeLoop
    @Specialization(
        guards = { "applicableMethodsEqual(applicableMethodsArg, applicableMethods)" }
    )
    Object doCached(
        GenericMethodApplicableMethods applicableMethodsArg,
        Object[] args,
        @Cached("applicableMethodsArg") GenericMethodApplicableMethods applicableMethods,
        @Cached ISLISPFunctionDispatchNode functionDispatchNode
    ) {
        if (applicableMethods.aroundMethods().size() != 0) {
            var newApplicableMethods = new GenericMethodApplicableMethods(
                applicableMethods.primaryMethods(),
                applicableMethods.aroundMethods().drop(1),
                applicableMethods.beforeMethods(),
                applicableMethods.afterMethods()
            );
            var closure = new Closure(null, newApplicableMethods, args);
            var fun = new LispFunction(closure, applicableMethods.aroundMethods().get(0));
            return functionDispatchNode.executeDispatch(fun, args);
        } else {
            for (int i = applicableMethods.beforeMethods().start(); i < applicableMethods.beforeMethods().end(); i++) {
                var fun = new LispFunction(applicableMethods.beforeMethods().els()[i]);
                functionDispatchNode.executeDispatch(fun, args);
            }
            var newApplicableMethods = new GenericMethodApplicableMethods(
                applicableMethods.primaryMethods().drop(1),
                new ArraySlice<>(new CallTarget[0]),
                new ArraySlice<>(new CallTarget[0]),
                new ArraySlice<>(new CallTarget[0])
            );
            //TODO handle when primary methods empty?
            var closure = new Closure(null, newApplicableMethods, args);
            var fun = new LispFunction(closure, applicableMethods.primaryMethods().get(0));
            var result = functionDispatchNode.executeDispatch(fun, args);
            for (int i = applicableMethods.afterMethods().start(); i < applicableMethods.afterMethods().end(); i++) {
                var afterFun = new LispFunction(applicableMethods.afterMethods().els()[i]);
                functionDispatchNode.executeDispatch(afterFun, args);
            }
            return result;
        }
    }

    boolean applicableMethodsEqual(GenericMethodApplicableMethods m1, GenericMethodApplicableMethods m2) {
        return arraySliceEqual(m1.primaryMethods(), m2.primaryMethods())
            && arraySliceEqual(m1.aroundMethods(), m2.aroundMethods())
            && arraySliceEqual(m1.beforeMethods(), m2.beforeMethods())
            && arraySliceEqual(m1.afterMethods(), m2.afterMethods());
    }

    boolean arraySliceEqual(ArraySlice s1, ArraySlice s2) {
        if (s1.size() != s2.size()) {
            return false;
        }
        for (var i = 0; i < s1.size(); i++) {
            if (s1.get(i) != s2.get(i)) {
                return false;
            }
        }
        return true;
    }

    @Specialization
    Object doUncached(
        GenericMethodApplicableMethods applicableMethods,
        Object[] args,
        @Cached ISLISPFunctionDispatchNode functionDispatchNode
    ) {
        if (applicableMethods.aroundMethods().size() != 0) {
            var newApplicableMethods = new GenericMethodApplicableMethods(
                applicableMethods.primaryMethods(),
                applicableMethods.aroundMethods().drop(1),
                applicableMethods.beforeMethods(),
                applicableMethods.afterMethods()
            );
            var closure = new Closure(null, newApplicableMethods, args);
            var fun = new LispFunction(closure, applicableMethods.aroundMethods().get(0));
            return functionDispatchNode.executeDispatch(fun, args);
        } else {
            for (int i = applicableMethods.beforeMethods().start(); i < applicableMethods.beforeMethods().end(); i++) {
                var fun = new LispFunction(applicableMethods.beforeMethods().els()[i]);
                functionDispatchNode.executeDispatch(fun, args);
            }
            var newApplicableMethods = new GenericMethodApplicableMethods(
                applicableMethods.primaryMethods().drop(1),
                new ArraySlice<>(new CallTarget[0]),
                new ArraySlice<>(new CallTarget[0]),
                new ArraySlice<>(new CallTarget[0])
            );
            //TODO handle when primary methods empty?
            var closure = new Closure(null, newApplicableMethods, args);
            var fun = new LispFunction(closure, applicableMethods.primaryMethods().get(0));
            var result = functionDispatchNode.executeDispatch(fun, args);
            for (int i = applicableMethods.afterMethods().start(); i < applicableMethods.afterMethods().end(); i++) {
                var afterFun = new LispFunction(applicableMethods.afterMethods().els()[i]);
                functionDispatchNode.executeDispatch(afterFun, args);
            }
            return result;
        }
    }


}
