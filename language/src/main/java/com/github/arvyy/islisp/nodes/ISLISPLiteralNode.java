package com.github.arvyy.islisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Yields literal value.
 */
public class ISLISPLiteralNode extends ISLISPExpressionNode {

    private final Object value;

    /**
     * Create literal node.
     *
     * @param value value to be returned on evaluation.
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPLiteralNode(Object value, SourceSection sourceSection) {
        super(sourceSection);
        this.value = value;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return value;
    }

}
