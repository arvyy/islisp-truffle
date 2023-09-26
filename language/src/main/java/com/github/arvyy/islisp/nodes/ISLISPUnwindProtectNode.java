package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPUnwindProtectNode extends ISLISPExpressionNode {

    @Child
    private final ISLISPExpressionNode expression;

    @Children
    private final ISLISPExpressionNode[] cleanups;

    public ISLISPUnwindProtectNode(
            ISLISPExpressionNode expressionNode,
            ISLISPExpressionNode[] cleanups,
            SourceSection sourceSection
    ) {
        super(sourceSection);
        this.expression = expressionNode;
        this.cleanups = cleanups;
    }

    @Override
    @ExplodeLoop
    public Value executeGeneric(VirtualFrame frame) {
        Value result;
        try {
            result = expression.executeGeneric(frame);
        } catch (RuntimeException e) {
            for (int i = 0; i < cleanups.length; i++) {
                cleanups[i].executeGeneric(frame);
            }
            throw e;
        }
        for (int i = 0; i < cleanups.length; i++) {
            cleanups[i].executeGeneric(frame);
        }
        return result;
    }

}
