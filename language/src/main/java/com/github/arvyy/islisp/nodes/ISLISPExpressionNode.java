package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.parser.ParserContext;
import com.github.arvyy.islisp.runtime.DebuggerScope;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.interop.NodeLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Base abstract node for all ISLISP nodes that have to yield a value.
 */
@TypeSystemReference(ISLISPTypes.class)
@GenerateWrapper
@ReportPolymorphism
@ExportLibrary(value = NodeLibrary.class)
public abstract class ISLISPExpressionNode extends Node implements InstrumentableNode {

    private final boolean isDefinitionNode;
    private final SourceSection sourceSection;
    @CompilerDirectives.CompilationFinal
    private boolean isRootBody;

    @CompilerDirectives.CompilationFinal
    private boolean internal = false;

    private boolean parserContextKnown = false;
    private ParserContext parserContext;

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
        return !internal;
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        if (getSourceSection() != null && tag == StandardTags.ExpressionTag.class) {
            return true;
        }
        if (getSourceSection() != null && tag == StandardTags.StatementTag.class) {
            return true;
        }
        return tag == StandardTags.RootBodyTag.class && isRootBody;
    }

    /**
     *
     * @return parsing context active during parse of this node
     */
    @CompilerDirectives.TruffleBoundary
    public ParserContext getParserContext() {
        if (parserContext == null && !parserContextKnown) {
            parserContextKnown = true;
            Node node = getParent();
            while (node != null) {
                if (node instanceof ISLISPExpressionNode islispNode) {
                    parserContext = islispNode.getParserContext();
                    break;
                } else {
                    node = node.getParent();
                }
            }
        }
        return parserContext;
    }

    /**
     * Set parserContext that was active during parse of the node.
     * Mostly set on nodes that change lexical scope; if unset, parserContext
     * is recursively looked up in parent nodes.
     * @param parserContext
     */
    public void setParserContext(ParserContext parserContext) {
        this.parserContext = parserContext;
    }

    @ExportMessage
    boolean hasScope(Frame f) {
        return getParserContext() != null;
    }

    @ExportMessage
    Object getScope(Frame frame, boolean nodeEnter) {
        return getScopeBoundary(frame.materialize());
    }


    @CompilerDirectives.TruffleBoundary
    Object getScopeBoundary(MaterializedFrame frame) {
        return new DebuggerScope(frame, getParserContext().getLocalScopeVariables());
    }

}
