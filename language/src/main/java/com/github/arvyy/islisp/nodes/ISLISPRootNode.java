package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.RootNode;

public class ISLISPRootNode extends RootNode {

    @Children
    private final ISLISPExpressionNode[] expressionNodes;

    public ISLISPRootNode(
            TruffleLanguage<?> language,
            ISLISPExpressionNode[] expressionNodes,
            FrameDescriptor frameDescriptor
    ) {
        super(language, frameDescriptor);
        this.expressionNodes = expressionNodes;
    }

    @Override
    @ExplodeLoop
    public Object execute(VirtualFrame frame) {
        for (int i = 0; i < expressionNodes.length - 1; i++) {
            expressionNodes[i].executeGeneric(frame);
        }
        if (expressionNodes.length != 0) {
            return expressionNodes[expressionNodes.length - 1].executeGeneric(frame);
        } else {
            return ISLISPContext.get(this).getNil();
        }
    }

}
