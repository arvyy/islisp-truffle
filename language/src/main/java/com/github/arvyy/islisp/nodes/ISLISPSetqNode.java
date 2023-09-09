package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPSetqNode extends ISLISPExpressionNode {

    private final int frameIndex;
    private final int frameSlot;
    @Child
    ISLISPExpressionNode expression;

    public ISLISPSetqNode(int frameIndex, int frameSlot, ISLISPExpressionNode expression, SourceSection sourceSection) {
        super(sourceSection);
        this.frameIndex = frameIndex;
        this.frameSlot = frameSlot;
        this.expression = expression;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        Frame f = frame;
        for (int i = 0; i < frameIndex; i++) {
            f = ((Closure) f.getArguments()[0]).frame();
        }
        var value = expression.executeGeneric(frame);
        f.setObject(frameSlot, value);
        return value;
    }

}