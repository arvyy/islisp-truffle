package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.LoopNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RepeatingNode;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `for` syntax.
 */
public class ISLISPForNode extends ISLISPExpressionNode {

    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private final int[] variableSlots;

    @Child
    LoopNode loopNode;

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
        this.resultBody = resultBody;
        var repeatNode = new RepeatingForNode();
        repeatNode.variableInitializers = variableInitializers;
        repeatNode.variableStepExpressions = variableStepExpressions;
        repeatNode.body = body;
        repeatNode.testExpression = testExpression;
        repeatNode.frameSetters = new ISLISPFrameSetter[variableInitializers.length];
        for (int i = 0; i < repeatNode.frameSetters.length; i++) {
            repeatNode.frameSetters[i] = ISLISPFrameSetterNodeGen.create();
        }
        loopNode = Truffle.getRuntime().createLoopNode(repeatNode);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        ((RepeatingForNode) loopNode.getRepeatingNode()).initVariableSlots(frame);
        loopNode.execute(frame);
        if (resultBody.length == 0) {
            return ISLISPContext.get(this).getNil();
        }
        return runResultStatements(frame);
    }


    @ExplodeLoop
    Object runResultStatements(VirtualFrame frame) {
        for (int i = 0; i < resultBody.length - 1; i++) {
            resultBody[i].executeGeneric(frame);
        }
        return resultBody[resultBody.length - 1].executeGeneric(frame);
    }

    class RepeatingForNode extends Node implements RepeatingNode {

        @Children
        ISLISPExpressionNode[] variableInitializers;

        @Children
        ISLISPExpressionNode[] variableStepExpressions;

        @Children
        ISLISPFrameSetter[] frameSetters;

        @Children
        ISLISPExpressionNode[] body;

        @Child
        ISLISPExpressionNode testExpression;
        private final Object[] newValues = new Object[variableSlots.length];
        @Override
        public boolean executeRepeating(VirtualFrame frame) {
            var test = testExpression.executeGeneric(frame);
            if (Utils.isNil(this, test)) {
                runIteration(frame);
                return true;
            }
            return false;
        }

        @ExplodeLoop
        void initVariableSlots(VirtualFrame frame) {
            for (int i = 0; i < variableSlots.length; i++) {
                frameSetters[i].execute(frame, variableInitializers[i].executeGeneric(frame), variableSlots[i]);
            }
        }

        @ExplodeLoop
        void runIteration(VirtualFrame frame) {
            for (int i = 0; i < body.length; i++) {
                body[i].executeGeneric(frame);
            }
            for (int i = 0; i < variableSlots.length; i++) {
                newValues[i] = variableStepExpressions[i].executeGeneric(frame);
            }
            for (int i = 0; i < variableSlots.length; i++) {
                frameSetters[i].execute(frame, newValues[i], variableSlots[i]);
            }
        }
    }

}
