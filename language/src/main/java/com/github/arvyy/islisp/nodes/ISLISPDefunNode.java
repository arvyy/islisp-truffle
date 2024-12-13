package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;

/**
 * Implements `defun` syntax for creating new user defined simple functions.
 */
public class ISLISPDefunNode extends ISLISPExpressionNode {

    final String module;
    final Symbol name;

    ISLISPRootNode functionNode;

    /**
     * Create defun node.
     *
     * @param module module name whose source's this node is part of
     * @param name function's symbol name
     * @param functionNode root node wrapping function body
     */
    public ISLISPDefunNode(String module, Symbol name, ISLISPRootNode functionNode) {
        super(true, functionNode.getSourceSection());
        this.module = module;
        this.name = name;
        this.functionNode = functionNode;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var fun = new LispFunction(
            new Closure(null, null, null),
            functionNode.getCallTarget(),
            false,
            ctx.getModule(module).shouldInline(name));
        ctx.registerFunction(module, name, fun);
        return name;
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        if (tag == StandardTags.StatementTag.class) {
            return true;
        }
        return super.hasTag(tag);
    }
}
