package com.github.arvyy.islisp.nodes;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;

public class ISLISPUserDefinedFunctionNode extends RootNode {

    @Child
    private ISLISPExpressionNode body;

    private final int[] namedArgumentSlots;

    protected ISLISPUserDefinedFunctionNode(
            TruffleLanguage<?> language,
            FrameDescriptor frameDescriptor,
            ISLISPExpressionNode body,
            int[] namedArgumentSlots
    ) {
        super(language, frameDescriptor);
        this.body = body;
        this.namedArgumentSlots = namedArgumentSlots;
    }

    @Override
    @ExplodeLoop
    public Object execute(VirtualFrame frame) {
        for (var i = 0; i < namedArgumentSlots.length; i++) {
            int slot = namedArgumentSlots[i];
            frame.setObject(slot, frame.getArguments()[i + 1]);
        }
        return body.executeGeneric(frame);
    }
}
