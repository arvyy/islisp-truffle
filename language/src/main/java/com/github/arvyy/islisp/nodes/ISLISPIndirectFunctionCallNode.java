package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPIndirectFunctionCallNode extends ISLISPExpressionNode {

    @Children
    private ISLISPExpressionNode[] arguments;

    @Child
    private ISLISPFunctionDispatchNode dispatchNode;

    public ISLISPIndirectFunctionCallNode(ISLISPExpressionNode[] arguments, SourceSection sourceSection) {
        super(sourceSection);
        this.arguments = arguments;
        dispatchNode = ISLISPFunctionDispatchNodeGen.create();
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        var argValues = new Value[arguments.length];
        for (int i = 0; i < argValues.length; i++) {
            argValues[i] = arguments[i].executeGeneric(frame);
        }
        var function = argValues[0];
        var functionArgs = new Value[arguments.length - 1];
        System.arraycopy(argValues, 1, functionArgs, 0, functionArgs.length);
        return dispatchNode.executeDispatch(function, functionArgs);
    }
}
