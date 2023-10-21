package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPReturnFromException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `block` syntax. At parsetime each block is associated with a unique id. return-from
 * is implemented through a control flow exception that carries the same id as the block it's returning to.
 */
public class ISLISPBlockNode extends ISLISPExpressionNode {

    private final int blockId;

    @Children
    private final ISLISPExpressionNode[] expressionNodes;

    /**
     * Create block node.
     *
     * @param blockId unique id to match against return-from nodes.
     * @param expressionNodes body of the block
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPBlockNode(int blockId, ISLISPExpressionNode[] expressionNodes, SourceSection sourceSection) {
        super(sourceSection);
        this.blockId = blockId;
        this.expressionNodes = expressionNodes;
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        if (expressionNodes.length == 0) {
            return ISLISPContext.get(this).getNil();
        }
        try {
            for (int i = 0; i < expressionNodes.length - 1; i++) {
                expressionNodes[i].executeGeneric(frame);
            }
            return expressionNodes[expressionNodes.length - 1].executeGeneric(frame);
        } catch (ISLISPReturnFromException e) {
            if (blockId == e.getBlockId()) {
                return e.getValue();
            } else {
                throw e;
            }
        }
    }
}
