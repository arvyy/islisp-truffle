package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPReturnFromException;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPBlockNode extends ISLISPExpressionNode {

    private final int blockId;

    @Children
    private ISLISPExpressionNode[] expressionNodes;

    public ISLISPBlockNode(int blockId, ISLISPExpressionNode[] expressionNodes, SourceSection sourceSection) {
        super(sourceSection);
        this.blockId = blockId;
        this.expressionNodes = expressionNodes;
    }

    @Override
    @ExplodeLoop
    public Value executeGeneric(VirtualFrame frame) {
        if (expressionNodes.length == 0)
            return ISLISPContext.get(this).getNIL();
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
