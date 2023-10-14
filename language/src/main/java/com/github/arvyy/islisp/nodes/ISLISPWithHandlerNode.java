package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.exceptions.ISLISPNonContinuableCondition;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPWithHandlerNode extends ISLISPExpressionNode {

    @Child
    private ISLISPExpressionNode handlerFunctionExpression;

    @Children
    private ISLISPExpressionNode[] body;

    @Child
    ISLISPFunctionDispatchNode handlerDispatch;

    public ISLISPWithHandlerNode(
        ISLISPExpressionNode handlerFunctionExpression,
        ISLISPExpressionNode[] body,
        SourceSection sourceSection
    ) {
        super(sourceSection);
        this.handlerFunctionExpression = handlerFunctionExpression;
        this.body = body;
        handlerDispatch = ISLISPFunctionDispatchNodeGen.create();
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        if (handlerFunctionExpression.executeGeneric(frame) instanceof LispFunction handlerFunction)  {
            var ctx = ISLISPContext.get(this);
            ctx.pushHandler(handlerFunction);
            // double try, because ctx.popHandler has to happen before the catch of non-continuable exception
            try {
                try {
                    if (body.length == 0) {
                        return ctx.getNil();
                    } else {
                        for (var i = 0; i < body.length - 1; i++) {
                            body[i].executeGeneric(frame);
                        }
                        return body[body.length - 1].executeGeneric(frame);
                    }
                } finally {
                    ctx.popHandler();
                }
            } catch (ISLISPNonContinuableCondition e) {
                return handlerDispatch.executeDispatch(handlerFunction, new Object[]{e.getCondition()});
            }
        } else {
            throw new ISLISPError("Handler not a function", this);
        }
    }
}
