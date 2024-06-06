package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.oracle.truffle.api.TruffleSafepoint;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `for` syntax.
 */
public class ISLISPForNode extends ISLISPExpressionNode {

    private final int[] variableSlots;

    @Children
    ISLISPExpressionNode[] variableInitializers;

    @Children
    ISLISPExpressionNode[] variableStepExpressions;

    @Children
    ISLISPExpressionNode[] body;

    @Child
    ISLISPExpressionNode testExpression;

    @Children
    ISLISPExpressionNode[] resultBody;

    /**
     * Create for node.
     *
     * @param variableSlots variable slots in the active frame.
     * @param variableInitializers variable initialization expressions.
     * @param variableStepExpressions variable loop step expressions.
     * @param body expressions to execute in each iteration when test is falsy.
     * @param testExpression test expression which stops iteration when yielding nil.
     * @param resultBody expression to evaluate after iteration ended for final result.
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPForNode(
        int[] variableSlots,
        ISLISPExpressionNode[] variableInitializers,
        ISLISPExpressionNode[] variableStepExpressions,
        ISLISPExpressionNode[] body,
        ISLISPExpressionNode testExpression,
        ISLISPExpressionNode[] resultBody,
        SourceSection sourceSection
    ) {
        super(sourceSection);
        this.variableSlots = variableSlots;
        this.variableInitializers = variableInitializers;
        this.variableStepExpressions = variableStepExpressions;
        this.body = body;
        this.testExpression = testExpression;
        this.resultBody = resultBody;
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        var nil = ISLISPContext.get(this).getNil();
        for (int i = 0; i < variableSlots.length; i++) {
            frame.setObject(variableSlots[i], variableInitializers[i].executeGeneric(frame));
        }
        while (testExpression.executeGeneric(frame) == nil) {
            for (int i = 0; i < body.length; i++) {
                body[i].executeGeneric(frame);
            }
            Object[] newValues = new Object[variableSlots.length];
            for (int i = 0; i < variableSlots.length; i++) {
                newValues[i] = variableStepExpressions[i].executeGeneric(frame);
            }
            for (int i = 0; i < variableSlots.length; i++) {
                frame.setObject(variableSlots[i], newValues[i]);
            }
            TruffleSafepoint.poll(this);
        }
        if (resultBody.length == 0) {
            return nil;
        }
        for (int i = 0; i < resultBody.length - 1; i++) {
            resultBody[i].executeGeneric(frame);
        }
        return resultBody[resultBody.length - 1].executeGeneric(frame);
    }
}
