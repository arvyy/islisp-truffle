package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPTagbodyGoException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `tagbody` syntax.
 */
public class ISLISPTagBodyNode extends ISLISPExpressionNode {

    // list of tags present in this tagbody form
    private final int[] tagIds;

    // tag tagIds[i] points to expressions[tagPositions[i]]
    private final int[] tagPosition;

    @Children
    private ISLISPExpressionNode[] expressions;

    /**
     * Create tagbody node.
     *
     * @param tagIds array of tag ids for each possible jump point
     * @param tagPosition possition array, where tagId[i] points to exresspion[tagPosition[i]]
     * @param expressions array of body expressions
     * @param sourceSection corresponding source section to this node
     */
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
    public Object executeGeneric(VirtualFrame frame) {
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
