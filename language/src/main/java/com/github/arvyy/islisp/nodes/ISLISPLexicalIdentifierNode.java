package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.Closure;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements identifier lookup from closure (but not global).
 */
public class ISLISPLexicalIdentifierNode extends ISLISPExpressionNode {

    private final int frameIndex;
    private final int frameSlot;

    @Child
    FrameGetter frameGetter;

    /**
     * Create identifier lookup node.
     *
     * @param frameIndex the amount of frames that need to be unrolled to reach this identifier's slot.
     * @param frameSlot frame slot index (in the right frame)
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPLexicalIdentifierNode(int frameIndex, int frameSlot, SourceSection sourceSection) {
        super(sourceSection);
        this.frameIndex = frameIndex;
        this.frameSlot = frameSlot;
        frameGetter = ISLISPLexicalIdentifierNodeFactory.FrameGetterNodeGen.create();
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        Frame f = frame;
        for (int i = 0; i < frameIndex; i++) {
            f = ((Closure) f.getArguments()[0]).frame();
        }
        return frameGetter.execute(f, frameSlot);
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        if (tag == StandardTags.ReadVariableTag.class) {
            return true;
        }
        return super.hasTag(tag);
    }

    @Override
    public Object getNodeObject() {
        return super.getNodeObject();
    }

    abstract static class FrameGetter extends Node {

        abstract Object execute(Frame frame, int slot);

        @Specialization(limit = "1", guards = "frame.isInt(slot)")
        int getInt(Frame frame, int slot) {
            return frame.getInt(slot);
        }

        @Specialization(limit = "1", guards = "frame.isDouble(slot)")
        double getDouble(Frame frame, int slot) {
            return frame.getDouble(slot);
        }

        @Specialization(limit = "1", guards = "frame.isObject(slot)")
        Object getObject(Frame frame, int slot) {
            return frame.getObject(slot);
        }

        @Fallback
        Object getValue(Frame frame, int slot) {
            return frame.getValue(slot);
        }

    }

}
