package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.ValueReference;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `dynamic-let` syntax for parameterizing dynamically scoped variables
 * with values for the duration of the block.
 */
public class ISLISPDynamicLetNode extends ISLISPExpressionNode {

    private final String module;
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private ValueReference[] vars;
    private final Symbol[] symbols;

    @Children
    ISLISPExpressionNode[] initializers;

    @Children
    ISLISPExpressionNode[] body;

    /**
     * Create dynamic-let node.
     *
     * @param module module name whose source's this node is part of
     * @param symbols locally bound variable names
     * @param initializers initialization expression for each variable
     * @param body body of the let
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPDynamicLetNode(
            String module,
            Symbol[] symbols,
            ISLISPExpressionNode[] initializers,
            ISLISPExpressionNode[] body,
            SourceSection sourceSection
    ) {
        super(sourceSection);
        this.module = module;
        this.symbols = symbols;
        this.initializers = initializers;
        this.body = body;
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        if (vars == null) {
            vars = new ValueReference[symbols.length];
            for (int i = 0; i < vars.length; i++) {
                var existing = ctx.lookupDynamicVar(module, symbols[i]);
                if (existing == null) {
                    // TODO granulize source section
                    vars[i] = new ValueReference(getSourceSection());
                    vars[i].setValue(null);
                    ctx.registerDynamicVar(module, symbols[i], vars[i]);
                } else {
                    vars[i] = existing;
                }
            }
        }
        var oldValues = new Object[vars.length];
        var values = new Object[vars.length];
        for (int i = 0; i < values.length; i++) {
            values[i] = initializers[i].executeGeneric(frame);
            oldValues[i] = vars[i].getValue();
        }
        for (int i = 0; i < vars.length; i++) {
            vars[i].setValue(values[i]);
        }
        try {
            if (body.length == 0) {
                return ctx.getNil();
            }
            for (int i = 0; i < body.length - 1; i++) {
                body[i].executeGeneric(frame);
            }
            return body[body.length - 1].executeGeneric(frame);
        } finally {
            for (int i = 0; i < vars.length; i++) {
                vars[i].setValue(oldValues[i]);
            }
        }
    }
}
