package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;

public class ISLISPRootNode extends RootNode {

    @Children
    private ISLISPExpressionNode[] expressionNodes;

    public ISLISPRootNode(TruffleLanguage<?> language, ISLISPExpressionNode[] expressionNodes) {
        super(language);
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
            return Symbol.NIL;
        }
    }
}