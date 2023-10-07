package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPContinueException;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPFunctionDispatchNode;
import com.github.arvyy.islisp.nodes.ISLISPFunctionDispatchNodeGen;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class BuiltinSignalCondition extends RootNode {

    @Child
    ISLISPFunctionDispatchNode dispatchNode;

    protected BuiltinSignalCondition(TruffleLanguage<?> language) {
        super(language);
        dispatchNode = ISLISPFunctionDispatchNodeGen.create();
    }

    @Override
    public Object execute(VirtualFrame frame) {
        // TODO handle non-continuable differently
        // TODO validate condition is actually condition
        var conditionValue = frame.getArguments()[1];
        var continuable = frame.getArguments()[2];
        var ctx = ISLISPContext.get(this);
        var handler = ctx.popHandler();
        try {
            dispatchNode.executeDispatch(handler, new Object[] { conditionValue });
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
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new BuiltinSignalCondition(lang).getCallTarget());
    }

}