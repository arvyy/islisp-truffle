package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPLambdaNode extends ISLISPExpressionNode {

    @Child
    ISLISPUserDefinedFunctionNode functionNode;

    public ISLISPLambdaNode(FrameDescriptor frameDescriptor, int[] namedArgumentSlots, ISLISPExpressionNode body, SourceSection sourceSection) {
        super(sourceSection);
        var ctx = ISLISPContext.get(this);
        functionNode = new ISLISPUserDefinedFunctionNode(ctx.getLanguage(), frameDescriptor, body, namedArgumentSlots, sourceSection);
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        return new LispFunction(frame.materialize(), functionNode.getCallTarget());
    }
}
