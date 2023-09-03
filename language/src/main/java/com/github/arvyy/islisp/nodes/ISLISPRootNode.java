package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPRootNode extends RootNode {

    private final SourceSection sourceSection;

    @Children
    private ISLISPExpressionNode[] expressionNodes;

    public ISLISPRootNode(TruffleLanguage<?> language, ISLISPExpressionNode[] expressionNodes, FrameDescriptor frameDescriptor, SourceSection sourceSection) {
        super(language, frameDescriptor);
        this.expressionNodes = expressionNodes;
        this.sourceSection = sourceSection;
    }

    protected ISLISPRootNode(ISLISPRootNode copy) {
        super(copy.getLanguage(ISLISPTruffleLanguage.class));
        sourceSection = copy.sourceSection;
    }

    @Override
    public SourceSection getSourceSection() {
        return sourceSection;
    }

    @Override
    @ExplodeLoop
    public Object execute(VirtualFrame frame) {
        for (int i = 0; i < expressionNodes.length - 1; i++) {
            expressionNodes[i].executeGeneric(frame);
        }
        if (expressionNodes.length != 0) {
            return expressionNodes[expressionNodes.length - 1].executeGeneric(frame);
        } else {
            return ISLISPContext.get(this).getNIL();
        }
    }

    @Override
    public boolean isInstrumentable() {
        return true;
    }

    @Override
    public boolean isInternal() {
        return false;
    }
}