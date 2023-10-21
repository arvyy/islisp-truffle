package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `progn` syntax for grouping multiple statements together.
 */
public class ISLISPPrognNode extends ISLISPExpressionNode {

    @Children
    private final ISLISPExpressionNode[] body;

    /**
     * Create progn node.
     *
     * @param body body expressions
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPPrognNode(ISLISPExpressionNode[] body, SourceSection sourceSection) {
        super(sourceSection);
        this.body = body;
    }

    /**
     * @return body expressions
     */
    public ISLISPExpressionNode[] getBodyNodes() {
        return body;
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        if (body.length == 0) {
            return ISLISPContext.get(this).getNil();
        }
        for (int i = 0; i < body.length - 1; i++) {
            body[i].executeGeneric(frame);
        }
        return body[body.length - 1].executeGeneric(frame);
    }
}
