package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPContinueException;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.exceptions.ISLISPNonContinuableCondition;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.nodes.ISLISPFunctionDispatchNode;
import com.github.arvyy.islisp.nodes.ISLISPFunctionDispatchNodeGen;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class ISLISPSignalCondition extends RootNode {

    @Child
    ISLISPFunctionDispatchNode dispatchNode;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    protected ISLISPSignalCondition(TruffleLanguage<?> language) {
        super(language);
        dispatchNode = ISLISPFunctionDispatchNodeGen.create();
        errorSignalerNode = new ISLISPErrorSignalerNode();
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        if (frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length, 2, 2);
        }
        // TODO validate condition is actually condition
        var conditionValue = frame.getArguments()[1];
        var continuable = frame.getArguments()[2] != ctx.getNil();
        if (continuable) {
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

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPSignalCondition(lang).getCallTarget());
    }

}
