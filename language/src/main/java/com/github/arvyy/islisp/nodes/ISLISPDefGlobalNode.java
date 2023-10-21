package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `defglobal` syntax for defining global mutable variables.
 */
public class ISLISPDefGlobalNode extends ISLISPExpressionNode {

    private final Symbol name;
    @Child
    private ISLISPExpressionNode expression;

    /**
     * Create defglobal node.
     *
     * @param name variable name
     * @param expression initial value
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPDefGlobalNode(Symbol name, ISLISPExpressionNode expression, SourceSection sourceSection) {
        super(true, sourceSection);
        this.name = name;
        this.expression = expression;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        ISLISPContext.get(this).registerGlobalVar(name.identityReference(), expression.executeGeneric(frame), false);
        return name;
    }
}
