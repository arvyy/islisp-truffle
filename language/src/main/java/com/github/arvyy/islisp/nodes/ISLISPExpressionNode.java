package com.github.arvyy.islisp.nodes;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

@TypeSystemReference(ISLISPTypes.class)
@GenerateWrapper
public abstract class ISLISPExpressionNode extends Node implements InstrumentableNode {

    private final boolean isDefinitionNode;
    private final SourceSection sourceSection;
    @CompilerDirectives.CompilationFinal
    private boolean isRootBody;

    public ISLISPExpressionNode(SourceSection sourceSection) {
        isDefinitionNode = false;
        this.sourceSection = sourceSection;
    }

    public ISLISPExpressionNode(boolean isDefinitionNode, SourceSection sourceSection) {
        this.isDefinitionNode = isDefinitionNode;
        this.sourceSection = sourceSection;
    }

    public void markRootBody() {
        this.isRootBody = true;
    }

    @Override
    public SourceSection getSourceSection() {
        return sourceSection;
    }


    public abstract Object executeGeneric(VirtualFrame frame);

    public boolean isDefinitionNode() {
        return isDefinitionNode;
    }

    @Override
    public WrapperNode createWrapper(ProbeNode probe) {
        return new ISLISPExpressionNodeWrapper(getSourceSection(), this, probe);
    }

    @Override
    public boolean isInstrumentable() {
        return true;
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        if (tag == StandardTags.ExpressionTag.class) {
            return true;
        }
        if (tag == StandardTags.StatementTag.class) {
            return true;
        }
        return tag == StandardTags.RootBodyTag.class && isRootBody;
    }
}
