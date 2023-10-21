package com.github.arvyy.islisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements indirect function call, such as through funcall syntax.
 */
public class ISLISPIndirectFunctionCallNode extends ISLISPExpressionNode {

    @Child
    private ISLISPExpressionNode fn;

    @Children
    private ISLISPExpressionNode[] arguments;

    @Child
    private ISLISPFunctionDispatchNode dispatchNode;

    /**
     * Create indirect function call node.
     *
     * @param fn expression that must yield a function value
     * @param arguments expressions which when executed form the function arguments
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPIndirectFunctionCallNode(
            ISLISPExpressionNode fn,
            ISLISPExpressionNode[] arguments,
            SourceSection sourceSection
    ) {
        super(sourceSection);
        this.fn = fn;
        this.arguments = arguments;
        dispatchNode = ISLISPFunctionDispatchNodeGen.create();
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        var argValues = new Object[arguments.length];
        for (int i = 0; i < argValues.length; i++) {
            argValues[i] = arguments[i].executeGeneric(frame);
        }
        var functionValue = fn.executeGeneric(frame);
        return dispatchNode.executeDispatch(functionValue, argValues);
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        if (tag == StandardTags.CallTag.class) {
            return true;
        }
        return super.hasTag(tag);
    }
}
