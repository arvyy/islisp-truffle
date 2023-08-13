package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;

public class ISLISPDefunNode extends ISLISPExpressionNode {

    final String name;
    final FrameDescriptor frameDescriptor;
    final int[] namedArgumentSlots;

    @Child
    ISLISPExpressionNode body;

    public ISLISPDefunNode(String name, FrameDescriptor frameDescriptor, int[] namedArgumentSlots, ISLISPExpressionNode body) {
        super(true);
        this.name = name;
        this.frameDescriptor = frameDescriptor;
        this.namedArgumentSlots = namedArgumentSlots;
        this.body = body;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var fun = new ISLISPUserDefinedFunctionNode(ctx.getLanguage(), frameDescriptor, body, namedArgumentSlots);
        ctx.registerFunction(name, new LispFunction(null, fun.getCallTarget()));
        return new Symbol(name);
    }
}
