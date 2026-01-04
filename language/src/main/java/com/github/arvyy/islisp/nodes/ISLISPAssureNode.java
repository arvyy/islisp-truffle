package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.runtime.LispClass;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `assure` and `the` syntax.
 */
public class ISLISPAssureNode extends ISLISPExpressionNode {

    private final Symbol className;

    @Child
    private ISLISPExpressionNode expressionNode;

    @CompilerDirectives.CompilationFinal
    private LispClass lispClass;

    @CompilerDirectives.CompilationFinal
    private DirectCallNode instancepCall;

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    /**
     * Create assure node.
     *
     * @param className name in the class namespace.
     * @param expressionNode expression to evaluate for object value.
     * @param sourceSection relevant source section.
     */
    public ISLISPAssureNode(Symbol className, ISLISPExpressionNode expressionNode, SourceSection sourceSection) {
        super(sourceSection);
        this.className = className;
        this.expressionNode = expressionNode;
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        if (lispClass == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            lispClass = ctx.lookupClass(className);
            instancepCall = insert(DirectCallNode.create(
                ctx.lookupFunction("ROOT", ctx.namedSymbol("instancep"))
                    .callTarget()));
        }
        var obj = expressionNode.executeGeneric(frame);
        var isSubclass = instancepCall.call(null, obj, lispClass);
        if (Utils.isNil(this, isSubclass)) {
            return errorSignalerNode.signalWrongType(obj, lispClass);
        }
        return obj;
    }

}
