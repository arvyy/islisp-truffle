package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.ValueReference;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `dynamic` syntax to lookup current value of a dynamically scoped variable.
 */
public class ISLISPDynamicLookupNode extends ISLISPExpressionNode {

    private final String module;
    private final Symbol symbol;

    @CompilerDirectives.CompilationFinal
    private ValueReference valueReference;

    /**
     * Creaste dynamic variable lookup node.
     *
     * @param module module name whose source's this node is part of
     * @param symbol variable name
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPDynamicLookupNode(String module, Symbol symbol, SourceSection sourceSection) {
        super(sourceSection);
        this.module = module;
        this.symbol = symbol;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        if (valueReference == null) {
            valueReference = ISLISPContext.get(this).lookupDynamicVar(module, symbol.identityReference());
            if (valueReference == null) {
                throw new ISLISPError("Undefined dynamic variable", this);
            }
        }
        var value = valueReference.getValue();
        if (value == null) {
            throw new ISLISPError("Undefined dynamic variable", this);
        }
        return value;
    }
}
