package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `defconstant` syntax for creating global constants.
 */
public class ISLISPDefConstantNode extends ISLISPExpressionNode {

    private final String module;
    private final Symbol name;
    @Child
    private ISLISPExpressionNode expression;

    /**
     * Create defconstant node.
     *
     * @param module module name whose source's this node is part of
     * @param name costant name
     * @param expression constant value
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPDefConstantNode(
        String module,
        Symbol name,
        ISLISPExpressionNode expression,
        SourceSection sourceSection
    ) {
        super(true, sourceSection);
        this.module = module;
        this.name = name;
        this.expression = expression;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        ISLISPContext.get(this).registerGlobalVar(
            module, name.identityReference(), expression.executeGeneric(frame), true);
        return name;
    }
}
