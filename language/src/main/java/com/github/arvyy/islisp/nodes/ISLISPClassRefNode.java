package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispClass;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `class` syntax, allows retrieving class from the class namespace as a value.
 */
public class ISLISPClassRefNode extends ISLISPExpressionNode {

    private final Symbol name;
    @CompilerDirectives.CompilationFinal
    private LispClass clazz;

    /**
     * Create class node.
     *
     * @param name class name
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPClassRefNode(Symbol name, SourceSection sourceSection) {
        super(sourceSection);
        this.name = name;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        if (clazz == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            clazz = ISLISPContext.get(this).lookupClass(name.identityReference());
        }
        return clazz;
    }

}
