package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.ValueReference;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements a global variable lookup.
 */
public class ISLISPGlobalIdentifierNode extends ISLISPExpressionNode {

    @CompilerDirectives.CompilationFinal
    private ValueReference valueReference;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    private final String module;
    private final Symbol name;

    /**
     * Create global identifier lookup node.
     *
     * @param module module name whose source's this node is part of
     * @param name variable name
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPGlobalIdentifierNode(String module, Symbol name, SourceSection sourceSection) {
        super(sourceSection);
        this.module = module;
        this.name = name;
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        if (valueReference == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            valueReference = ISLISPContext.get(this).lookupGlobalVar(module, name.identityReference());
            if (valueReference == null) {
                return errorSignalerNode.signalUnboundVariable(name);
            }
        }
        return valueReference.getValue();
    }
}
