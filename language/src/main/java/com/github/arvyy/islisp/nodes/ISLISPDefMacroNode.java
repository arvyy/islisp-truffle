package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Implements `defmacro` syntax for creating syntax-macros.
 * Functionally similar to simple function except it has a slightly different storage mechanism.
 */
public class ISLISPDefMacroNode extends ISLISPExpressionNode {

    ISLISPDefunNode defun;

    /**
     * Create defmacro node.
     *
     * @param defun function definition for the macro.
     */
    public ISLISPDefMacroNode(ISLISPDefunNode defun) {
        super(true, null);
        this.defun = defun;
    }

    @Override
    public boolean isInstrumentable() {
        return false;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        //CompilerDirectives.transferToInterpreter();
        var ctx = ISLISPContext.get(this);
        ctx.registerMacro(defun.name.identityReference(), new LispFunction(defun.functionNode.getCallTarget()));
        return defun.name;
    }
}
