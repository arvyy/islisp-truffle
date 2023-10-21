package com.github.arvyy.islisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements special case when a `lambda` form is immediately invoked.
 */
public class ISLISPDirectLambdaCallNode extends ISLISPExpressionNode {

    @Child
    private ISLISPLambdaNode lambdaNode;

    @Children
    private ISLISPExpressionNode[] arguments;

    @Child
    private ISLISPFunctionDispatchNode dispatchNode;

    /**
     * Create direct lambda call.
     *
     * @param lambdaNode lambda node
     * @param arguments expressions to be used as arguments
     * @param sourceSection corresponding source section to this node
     */
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
    public Object executeGeneric(VirtualFrame frame) {
        var function = lambdaNode.executeGeneric(frame);
        var argValues = new Object[arguments.length];
        for (int i = 0; i < argValues.length; i++) {
            argValues[i] = arguments[i].executeGeneric(frame);
        }
        return dispatchNode.executeDispatch(function, argValues);
    }
}
