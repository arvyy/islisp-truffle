package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.parser.ModuleSource;
import com.github.arvyy.islisp.parser.Parser;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Helper node for deferring full parsing (which requires running user code) until runtime.
 */
public class ISLISPModuleNode extends ISLISPExpressionNode {

    private final ModuleSource source;
    private final Parser parser;

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

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        parser.ensureRequiresLoaded(source.requires());
        var ctx = ISLISPContext.get(this);
        if (ctx.getModule(source.name()) == null) {
            ctx.createModule(source.name(), source.requires(), source.provides());
        }
        return parser.expandAndExecute(source.name(), source.content(), expr -> {
            // insert exprs so that they get transitively instrumented
            insert(expr);
        });
    }
}
