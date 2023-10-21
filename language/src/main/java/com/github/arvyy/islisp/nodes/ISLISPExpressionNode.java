package com.github.arvyy.islisp.nodes;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Base abstract node for all ISLISP nodes that have to yield a value.
 */
@TypeSystemReference(ISLISPTypes.class)
@GenerateWrapper
public abstract class ISLISPExpressionNode extends Node implements InstrumentableNode {

    private final boolean isDefinitionNode;
    private final SourceSection sourceSection;
    @CompilerDirectives.CompilationFinal
    private boolean isRootBody;

    /**
     * Create node with definition flag = false and source information.
     *
     * @param sourceSection source fragment corresponding to this node
     */
    public ISLISPExpressionNode(SourceSection sourceSection) {
        isDefinitionNode = false;
        this.sourceSection = sourceSection;
    }

    /**
     * Create node with definition flag and source information.
     *
     * @param isDefinitionNode if this is a definition node. See @isDefinitionNode()
     * @param sourceSection source fragment corresponding to this node
     */
    public ISLISPExpressionNode(boolean isDefinitionNode, SourceSection sourceSection) {
        this.isDefinitionNode = isDefinitionNode;
        this.sourceSection = sourceSection;
    }

    /**
     * Specifiy this node is a function root body
     * to return relevant truffle tag.
     */
    public void markRootBody() {
        this.isRootBody = true;
    }

    @Override
    public SourceSection getSourceSection() {
        return sourceSection;
    }


    /**
     * Base method for executing the node to yield a value.
     *
     * @param frame current function frame. frame.getArgumets()[0] always contains closure context.
     * @return evaluation result value.
     */
    public abstract Object executeGeneric(VirtualFrame frame);

    /**
     * Used to determine if the node at top level defines.
     * Used in macro expansion to only execute necessary definitions to be available
     * but not simple (and potentially side effectful) code.
     *
     * @return true if this is definition node.
     */
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
