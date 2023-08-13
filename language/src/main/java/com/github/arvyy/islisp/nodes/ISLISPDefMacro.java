package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;

public class ISLISPDefMacro extends ISLISPExpressionNode {

    @Child
    ISLISPDefunNode defun;

    public ISLISPDefMacro(ISLISPDefunNode defun) {
        super(true);
        this.defun = defun;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        CompilerAsserts.neverPartOfCompilation();
        var ctx = ISLISPContext.get(this);
        var fun = new ISLISPUserDefinedFunctionNode(ctx.getLanguage(), defun.frameDescriptor, defun.body, defun.namedArgumentSlots);
        ctx.registerMacro(defun.name, new LispFunction(null, fun.getCallTarget()));
        return new Symbol(defun.name);
    }
}
