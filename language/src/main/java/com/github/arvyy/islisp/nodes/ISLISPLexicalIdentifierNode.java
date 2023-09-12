package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPLexicalIdentifierNode extends ISLISPExpressionNode {

    private final int frameIndex;
    private final int frameSlot;

    public ISLISPLexicalIdentifierNode(int frameIndex, int frameSlot, SourceSection sourceSection) {
        super(sourceSection);
        this.frameIndex = frameIndex;
        this.frameSlot = frameSlot;
    }

    @Override
    @ExplodeLoop
    public Value executeGeneric(VirtualFrame frame) {
        Frame f = frame;
        for (int i = 0; i < frameIndex; i++) {
            f = ((Closure) f.getArguments()[0]).frame();
        }
        return (Value) f.getObject(frameSlot);
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        if (tag == StandardTags.ReadVariableTag.class)
            return true;
        return super.hasTag(tag);
    }

    @Override
    public Object getNodeObject() {
        return super.getNodeObject();
    }
}
