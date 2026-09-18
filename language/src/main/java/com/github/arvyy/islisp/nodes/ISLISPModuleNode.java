package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.parser.ModuleSource;
import com.github.arvyy.islisp.parser.Parser;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;

import java.nio.file.Path;
import java.util.ArrayList;

/**
 * Helper node for deferring full parsing (which requires running user code) until runtime.
 */
public class ISLISPModuleNode extends ISLISPExpressionNode {

    private final ModuleSource source;
    private final Parser parser;

    @Child
    DirectCallNode nonDefinitionExecutionNode = null;

    @CompilerDirectives.CompilationFinal
    private boolean initialized = false;

    /**
     * Create macro expansion node.
     * @param parser reference to parser to execute proper parsing on invocation
     * @param moduleSource top level user code
     */
    public ISLISPModuleNode(Parser parser, ModuleSource moduleSource) {
        super(moduleSource.sourceSection());
        this.parser = parser;
        this.source = moduleSource;
    }

    public void initialize() {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        parser.ensureRequiresLoaded(source.requires());
        var ctx = ISLISPContext.get(this);
        if (ctx.getModule(source.name()) == null) {
            ctx.createModule(source.name(), source.sourceSection(), source.requires(), source.provides());
        }
        var nonDefinitionRoot = parser.expandAndExecuteDefinitions(source.name(), source.content());
        nonDefinitionExecutionNode = DirectCallNode.create(nonDefinitionRoot.getCallTarget());
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        if (!initialized) {
            initialized = true;
            initialize();
        }
        nonDefinitionExecutionNode.call();
        return ISLISPContext.get(this).getNil();
    }
}
