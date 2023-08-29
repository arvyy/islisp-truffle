package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPIndirectFunctionCallNode extends ISLISPExpressionNode {

    @Child
    private ISLISPExpressionNode fn;

    @Children
    private ISLISPExpressionNode[] arguments;

    @Child
    private ISLISPFunctionDispatchNode dispatchNode;

    public ISLISPIndirectFunctionCallNode(ISLISPExpressionNode fn, ISLISPExpressionNode[] arguments, SourceSection sourceSection) {
        super(sourceSection);
        this.fn = fn;
        this.arguments = arguments;
        dispatchNode = ISLISPFunctionDispatchNodeGen.create();
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        var argValues = new Value[arguments.length];
        for (int i = 0; i < argValues.length; i++) {
            argValues[i] = arguments[i].executeGeneric(frame);
        }
        var functionValue = fn.executeGeneric(frame);
        return dispatchNode.executeDispatch(functionValue, argValues);
    }
}
