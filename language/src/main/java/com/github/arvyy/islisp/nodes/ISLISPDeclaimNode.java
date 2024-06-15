package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.parser.Declaration;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

import java.util.List;

/**
 * Implements `declaim` syntax for attaching meta-info about procedures.
 */
public class ISLISPDeclaimNode extends ISLISPExpressionNode {

    private final String module;
    private final List<Declaration> declarations;

    /**
     * Create declaim node.
     *
     * @param module module in which it takes effect
     * @param declarations list of declarations
     * @param sourceSection source section
     */
    public ISLISPDeclaimNode(String module, List<Declaration> declarations, SourceSection sourceSection) {
        super(sourceSection);
        this.declarations = declarations;
        this.module = module;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        executeBoundary();
        return ISLISPContext.get(this).getNil();
    }

    @CompilerDirectives.TruffleBoundary
    void executeBoundary() {
        ISLISPContext.get(this).getModule(module).addDeclarations(declarations);
    }

}
