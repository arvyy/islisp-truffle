package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.exceptions.ISLISPTagbodyGoException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `go` syntax for transfering control to specified part of a tagbody.
 */
public class ISLISPTagBodyGoNode extends ISLISPExpressionNode {

    private final int tagId;

    /**
     * Create tagbody go node.
     *
     * @param tagId the position to jump into
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPTagBodyGoNode(int tagId, SourceSection sourceSection) {
        super(sourceSection);
        this.tagId = tagId;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw new ISLISPTagbodyGoException(tagId);
    }
}
