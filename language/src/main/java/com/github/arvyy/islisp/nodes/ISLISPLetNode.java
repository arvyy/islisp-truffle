package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `let`, `let*`, `labels`, `flet` syntax for creating local lexical bindings.
 */
public class ISLISPLetNode extends ISLISPExpressionNode {

    final int[] variableSlots;

    @Children
    ISLISPExpressionNode[] variableInitializers;

    @Children
    ISLISPFrameSetter[] frameSetters;

    @Children
    ISLISPExpressionNode[] body;

    /**
     * Create let node.
     *
     * @param variableSlots indeces of frame slots for locally introduced variables.
     * @param variableInitializers expressions to be used to initialize variables
     * @param body let's body
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPLetNode(
            int[] variableSlots,
            ISLISPExpressionNode[] variableInitializers,
            ISLISPExpressionNode[] body,
            SourceSection sourceSection
    ) {
        super(sourceSection);
        this.variableSlots = variableSlots;
        this.variableInitializers = variableInitializers;
        this.body = body;
        frameSetters = new ISLISPFrameSetter[variableSlots.length];
        for (int i = 0; i < frameSetters.length; i++) {
            frameSetters[i] = ISLISPFrameSetterNodeGen.create();
        }
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        for (int i = 0; i < variableSlots.length; i++) {
            frameSetters[i].execute(frame, variableInitializers[i].executeGeneric(frame), variableSlots[i]);
        }
        if (body.length == 0) {
            return ISLISPContext.get(this).getNil();
        }
        for (int i = 0; i < body.length - 1; i++) {
            body[i].executeGeneric(frame);
        }
        return body[body.length - 1].executeGeneric(frame);
    }
}
