package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `function` syntax that allows looking up function from the function namespace to be used as a value.
 */
public class ISLISPFunctionRefNode extends ISLISPExpressionNode {

    private final String module;
    private final Symbol name;

    @CompilerDirectives.CompilationFinal
    private LispFunction function;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    /**
     * Create function reference node.
     * @param module module name whose source's this node is part of
     * @param name function name in the function namespace.
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPFunctionRefNode(String module, Symbol name, SourceSection sourceSection) {
        super(sourceSection);
        this.module = module;
        this.name = name;
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        if (function == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            function = ISLISPContext.get(this).lookupFunction(module, name);
            if (function == null) {
                return errorSignalerNode.signalUndefinedFunction(name);
            }
        }
        return function;
    }
}
