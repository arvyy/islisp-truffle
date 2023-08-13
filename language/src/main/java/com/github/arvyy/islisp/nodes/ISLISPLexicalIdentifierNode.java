package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;

public class ISLISPLexicalIdentifierNode extends ISLISPExpressionNode {

    private final int frameIndex;
    private final int frameSlot;

    public ISLISPLexicalIdentifierNode(int frameIndex, int frameSlot) {
        this.frameIndex = frameIndex;
        this.frameSlot = frameSlot;
    }

    @Override
    @ExplodeLoop
    public Value executeGeneric(VirtualFrame frame) {
        Frame f = frame;
        for (int i = 0; i < frameIndex; i++) {
            f = (Frame) f.getArguments()[0];
        }
        return (Value) f.getObject(frameSlot);
    }
}
