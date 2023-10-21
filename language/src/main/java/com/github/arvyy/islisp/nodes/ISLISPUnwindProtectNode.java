package com.github.arvyy.islisp.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `unwind-protect` syntax for defining cleanup forms after execution of body.
 */
public class ISLISPUnwindProtectNode extends ISLISPExpressionNode {

    @Child
    private ISLISPExpressionNode expression;

    @Children
    private ISLISPExpressionNode[] cleanups;

    /**
     * Create unwind-protect node.
     *
     * @param expressionNode body expression
     * @param cleanups clean up expressions to be executed upon return or non-local control transfer
     * @param sourceSection corresponding source section to this node
     */
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
    public Object executeGeneric(VirtualFrame frame) {
        Object result;
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
