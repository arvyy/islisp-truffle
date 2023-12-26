package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.ValueReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `defdynamic` syntax for creating dynamically scoped variables.
 */
public class ISLISPDefDynamicNode extends ISLISPExpressionNode {

    private final String module;
    private final Symbol name;

    @Child
    private ISLISPExpressionNode initializer;

    /**
     * Create defdynamic node.
     *
     * @param name dynamic variable's name
     * @param initializer expression for variable's initial value
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPDefDynamicNode(String module, Symbol name, ISLISPExpressionNode initializer, SourceSection sourceSection) {
        super(true, sourceSection);
        this.module = module;
        this.name = name;
        this.initializer = initializer;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var dynamicVar = new ValueReference();
        dynamicVar.setValue(initializer.executeGeneric(frame));
        ctx.registerDynamicVar(module, name.identityReference(), dynamicVar);
        return name;
    }
}
