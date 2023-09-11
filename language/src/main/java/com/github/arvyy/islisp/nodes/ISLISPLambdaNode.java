package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPLambdaNode extends ISLISPExpressionNode {

    @Child
    ISLISPRootNode functionNode;

    public ISLISPLambdaNode(ISLISPRootNode functionNode) {
        super(functionNode.getSourceSection());
        this.functionNode = functionNode;
    }

    @Override
    public LispFunction executeGeneric(VirtualFrame frame) {
        return new LispFunction(frame.materialize(), functionNode.getCallTarget());
    }
}
