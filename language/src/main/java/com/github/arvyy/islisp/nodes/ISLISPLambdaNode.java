package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Implements `lambda` anonymous function.
 */
public class ISLISPLambdaNode extends ISLISPExpressionNode {

    @Child
    ISLISPRootNode functionNode;

    /**
     * Create lambda node.
     *
     * @param functionNode root node that acts as entry point to lambda execution.
     */
    public ISLISPLambdaNode(ISLISPRootNode functionNode) {
        super(functionNode.getSourceSection());
        this.functionNode = functionNode;
    }

    @Override
    public LispFunction executeGeneric(VirtualFrame frame) {
        return new LispFunction(frame.materialize(), functionNode.getCallTarget());
    }
}
