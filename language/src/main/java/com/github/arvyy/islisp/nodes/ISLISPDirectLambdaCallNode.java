package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPDirectLambdaCallNode extends ISLISPExpressionNode {

    @Child
    private final ISLISPLambdaNode lambdaNode;

    @Children
    private final ISLISPExpressionNode[] arguments;

    @Child
    private final ISLISPFunctionDispatchNode dispatchNode;

    public ISLISPDirectLambdaCallNode(
            ISLISPLambdaNode lambdaNode,
            ISLISPExpressionNode[] arguments,
            SourceSection sourceSection
    ) {
        super(sourceSection);
        this.lambdaNode = lambdaNode;
        this.arguments = arguments;
        this.dispatchNode = ISLISPFunctionDispatchNodeGen.create();
    }


    @Override
    @ExplodeLoop
    public Value executeGeneric(VirtualFrame frame) {
        var function = lambdaNode.executeGeneric(frame);
        var argValues = new Value[arguments.length];
        for (int i = 0; i < argValues.length; i++) {
            argValues[i] = arguments[i].executeGeneric(frame);
        }
        return dispatchNode.executeDispatch(function, argValues);
    }
}
