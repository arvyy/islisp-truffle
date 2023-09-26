package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPTagbodyGoException;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPTagBodyNode extends ISLISPExpressionNode {

    // list of tags present in this tagbody form
    private final int[] tagIds;

    // tag tagIds[i] points to expressions[tagPositions[i]]
    private final int[] tagPosition;

    @Children
    private ISLISPExpressionNode[] expressions;

    public ISLISPTagBodyNode(
            int[] tagIds,
            int[] tagPosition,
            ISLISPExpressionNode[] expressions,
            SourceSection sourceSection
    ) {
        super(sourceSection);
        this.tagIds = tagIds;
        this.tagPosition = tagPosition;
        this.expressions = expressions;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        int pos = 0;
        while (true) {
            try {
                if (pos >= expressions.length) {
                    return ISLISPContext.get(this).getNil();
                } else {
                    expressions[pos].executeGeneric(frame);
                    pos++;
                }
            } catch (ISLISPTagbodyGoException e) {
                int index = -1;
                for (int i = 0; i < tagIds.length; i++) {
                    if (tagIds[i] == e.getGoId()) {
                        index = i;
                        break;
                    }
                }
                // no such go id found -- possibly targetted at a different tagbody form. Rethrow
                if (index == -1) {
                    throw e;
                }
                pos = tagPosition[index];
            }
        }
    }
}
