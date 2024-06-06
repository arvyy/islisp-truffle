package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPInteractiveExitException;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Root node that wraps all the top level statements as well as user-defined functions.
 */
public class ISLISPRootNode extends RootNode {

    private String name;

    private SourceSection sourceSection;

    @Children
    private final ISLISPExpressionNode[] expressionNodes;

    /**
     * Create root node.
     *
     * @param language language reference
     * @param expressionNodes root body
     * @param frameDescriptor descriptor of known local variable locations
     */
    public ISLISPRootNode(
            TruffleLanguage<?> language,
            ISLISPExpressionNode[] expressionNodes,
            FrameDescriptor frameDescriptor
    ) {
        super(language, frameDescriptor);
        this.expressionNodes = expressionNodes;
    }

    /**
     * Set root node's name
     * @param name name
     */
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    public void setSourceSection(SourceSection sourceSection) {
        this.sourceSection = sourceSection;
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
            try {
                return expressionNodes[expressionNodes.length - 1].executeGeneric(frame);
            } catch (ISLISPInteractiveExitException e) {
                return e.getCondition();
            }
        } else {
            return ISLISPContext.get(this).getNil();
        }
    }

    @Override
    public boolean isCaptureFramesForTrace() {
        return true;
    }

    @Override
    public boolean isCloningAllowed() {
        return true;
    }

}
