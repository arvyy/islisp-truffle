package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements indirect function call, through funcall or apply syntax.
 */
public class ISLISPIndirectFunctionCallNode extends ISLISPExpressionNode {

    @Child
    private ISLISPExpressionNode fn;

    @Children
    private ISLISPExpressionNode[] arguments;

    @Child
    private ISLISPFunctionDispatchNode dispatchNode;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    private final boolean lastArgRest;

    /**
     * Create indirect function call node.
     *
     * @param fn expression that must yield a function value
     * @param arguments expressions which when executed form the function arguments
     * @param lastArgRest if last argument should be a list of remaining arguments that will be spliced
     *                    upon invocation.
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPIndirectFunctionCallNode(
            ISLISPExpressionNode fn,
            ISLISPExpressionNode[] arguments,
            boolean lastArgRest,
            SourceSection sourceSection
    ) {
        super(sourceSection);
        this.fn = fn;
        this.arguments = arguments;
        this.lastArgRest = lastArgRest;
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
        dispatchNode = ISLISPFunctionDispatchNodeGen.create();
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        Object[] argValues;
        if (lastArgRest) {
            Object[] restArgValues;
            var restValue = arguments[arguments.length - 1].executeGeneric(frame);
            try {
                restArgValues = Utils.readListAsArray(restValue);
            } catch (Utils.NotAList ignored) {
                return errorSignalerNode.signalWrongType(
                    restValue,
                    ISLISPContext.get(this).lookupClass("<list>"));
            }
            argValues = new Object[restArgValues.length + arguments.length - 1];
            for (int i = 0; i < arguments.length - 1; i++) {
                argValues[i] = arguments[i].executeGeneric(frame);
            }
            System.arraycopy(restArgValues, 0, argValues, arguments.length - 1, restArgValues.length);
        } else {
            argValues = new Object[arguments.length];
            for (int i = 0; i < argValues.length; i++) {
                var argument = arguments[i];
                argValues[i] = argument.executeGeneric(frame);
            }
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
