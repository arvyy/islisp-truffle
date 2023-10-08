package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.frame.VirtualFrame;

public class ISLISPDefMacroNode extends ISLISPExpressionNode {

    @Child
    ISLISPDefunNode defun;

    public ISLISPDefMacroNode(ISLISPDefunNode defun) {
        super(true, null);
        this.defun = defun;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        //CompilerDirectives.transferToInterpreter();
        var ctx = ISLISPContext.get(this);
        ctx.registerMacro(defun.name.identityReference(), new LispFunction(defun.functionNode.getCallTarget()));
        return defun.name;
    }
}
