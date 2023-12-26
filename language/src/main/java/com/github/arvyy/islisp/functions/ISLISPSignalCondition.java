package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPContinueException;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.exceptions.ISLISPNonContinuableCondition;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.nodes.ISLISPFunctionDispatchNode;
import com.github.arvyy.islisp.nodes.ISLISPFunctionDispatchNodeGen;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `signal-condition` function. In case a condition is continuable,
 * it invokes active handler function, pushing it to callstack. If condition is resumed,
 * a control flow except unwinds back to the signal-condition call. If the condition isn't continuable,
 * an exception is raised to unwind to the handler.
 */
public class ISLISPSignalCondition extends RootNode {

    @Child
    ISLISPFunctionDispatchNode dispatchNode;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    @Child
    DirectCallNode fillStacktrace;

    @Child
    DirectCallNode setContinuable;

    @Child
    DirectCallNode setStacktrace;

    ISLISPSignalCondition(TruffleLanguage<?> language) {
        super(language);
        dispatchNode = ISLISPFunctionDispatchNodeGen.create();
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        if (fillStacktrace == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var fillStacktraceFunction = ctx.lookupFunction(
                "ROOT",
                ctx.namedSymbol("fill-stacktrace").identityReference());
            fillStacktrace = insert(DirectCallNode.create(fillStacktraceFunction.callTarget()));
        }
        if (setContinuable == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var setContinuableFunction = ctx.lookupFunction(
                "ROOT",
                ctx.namedSymbol("set-condition-continuable").identityReference()
            );
            setContinuable = insert(DirectCallNode.create(setContinuableFunction.callTarget()));
        }
        if (setStacktrace == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var setStacktraceFunction = ctx.lookupFunction(
                "ROOT",
                ctx.namedSymbol("set-condition-stacktrace").identityReference()
            );
            setStacktrace = insert(DirectCallNode.create(setStacktraceFunction.callTarget()));
        }
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length, 2, 2);
        }
        // TODO validate condition is actually condition
        var conditionValue = frame.getArguments()[1];
        var shouldFill = fillStacktrace.call(null, conditionValue);
        if (shouldFill != ctx.getNil()) {
            setStacktrace.call(null, ISLISPCurrentStacktrace.currentStacktrace(), conditionValue);
        }
        var continuable = frame.getArguments()[2];
        setContinuable.call(null, continuable, conditionValue);
        if (continuable != ctx.getNil()) {
            var handler = ctx.popHandler();
            try {
                dispatchNode.executeDispatch(handler, new Object[]{conditionValue});
                throw new ISLISPError("Condition handler returned normally", this);
            } catch (ISLISPContinueException e) {
                if (e.getCondition() == conditionValue) {
                    return e.getValue();
                } else {
                    throw e;
                }
            } finally {
                ctx.pushHandler(handler);
            }
        } else {
            throw new ISLISPNonContinuableCondition(conditionValue);
        }
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPSignalCondition(lang).getCallTarget());
    }

}
