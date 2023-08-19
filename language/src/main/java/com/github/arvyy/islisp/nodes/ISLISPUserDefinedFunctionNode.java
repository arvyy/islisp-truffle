package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

@GenerateWrapper
public class ISLISPUserDefinedFunctionNode extends RootNode implements InstrumentableNode {

    private final SourceSection sourceSection;

    @Child
    private ISLISPExpressionNode body;

    private final int[] namedArgumentSlots;

    public ISLISPUserDefinedFunctionNode(
            TruffleLanguage<?> language,
            FrameDescriptor frameDescriptor,
            ISLISPExpressionNode body,
            int[] namedArgumentSlots,
            SourceSection sourceSection
    ) {
        super(language, frameDescriptor);
        this.body = body;
        this.namedArgumentSlots = namedArgumentSlots;
        this.sourceSection = sourceSection;
        body.markRootBody();
    }

    protected ISLISPUserDefinedFunctionNode(ISLISPUserDefinedFunctionNode other) {
        super(other.getLanguage(ISLISPTruffleLanguage.class), other.getFrameDescriptor());
        this.body = other.body;
        this.namedArgumentSlots = other.namedArgumentSlots;
        this.sourceSection = other.sourceSection;
    }

    @Override
    @ExplodeLoop
    public Object execute(VirtualFrame frame) {
        for (var i = 0; i < namedArgumentSlots.length; i++) {
            int slot = namedArgumentSlots[i];
            frame.setObject(slot, frame.getArguments()[i + 1]);
        }
        return body.executeGeneric(frame);
    }

    @Override
    public boolean isInstrumentable() {
        return false;
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        if (tag == StandardTags.RootTag.class)
            return true;
        return false;
    }

    @Override
    public WrapperNode createWrapper(ProbeNode probe) {
        return new ISLISPUserDefinedFunctionNodeWrapper(this, this, probe);
    }

    @Override
    public SourceSection getSourceSection() {
        return sourceSection;
    }
}
