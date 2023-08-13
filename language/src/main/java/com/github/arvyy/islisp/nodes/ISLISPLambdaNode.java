package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;

public class ISLISPLambdaNode extends ISLISPExpressionNode {

    private final FrameDescriptor frameDescriptor;
    private final int[] namedArgumentSlots;

    @Child
    private ISLISPExpressionNode body;

    public ISLISPLambdaNode(FrameDescriptor frameDescriptor, int[] namedArgumentSlots, ISLISPExpressionNode body) {
        this.frameDescriptor = frameDescriptor;
        this.namedArgumentSlots = namedArgumentSlots;
        this.body = body;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var fun = new ISLISPUserDefinedFunctionNode(ctx.getLanguage(), frameDescriptor, body, namedArgumentSlots);
        return new LispFunction(frame.materialize(), fun.getCallTarget());
    }
}
