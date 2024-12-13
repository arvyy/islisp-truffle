package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.ValueReference;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements `setq` syntax for setting value of either global or closure / local
 * lexically scoped variable.
 */
public class ISLISPSetqNode extends ISLISPExpressionNode {

    private final int frameIndex;
    private final int frameSlot;

    private final String module;
    private final Symbol name;

    @CompilerDirectives.CompilationFinal
    private ValueReference valueReference;

    @Child
    ISLISPExpressionNode expression;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    @Child
    ISLISPFrameSetter frameSetter;

    /**
     * Create setq node for the global variable.
     *
     * @param module module name whose source's this node is part of
     * @param name global variable name
     * @param expression value expression
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPSetqNode(String module, Symbol name, ISLISPExpressionNode expression, SourceSection sourceSection) {
        super(sourceSection);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
        this.module = module;
        this.name = name;
        this.frameIndex = -1;
        this.frameSlot = -1;
        this.expression = expression;
    }

    /**
     * Create setq node for a local / closure variable.
     *
     * @param module module name whose source's this node is part of
     * @param frameIndex frame index
     * @param frameSlot frame slot in appropriate frame
     * @param expression value expression
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPSetqNode(
        String module,
        int frameIndex,
        int frameSlot,
        ISLISPExpressionNode expression,
        SourceSection sourceSection
    ) {
        super(sourceSection);
        this.module = module;
        this.name = null;
        this.frameIndex = frameIndex;
        this.frameSlot = frameSlot;
        this.expression = expression;
        frameSetter = ISLISPFrameSetterNodeGen.create();
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {
        if (name == null) {
            Frame f = frame;
            for (int i = 0; i < frameIndex; i++) {
                f = ((Closure) f.getArguments()[0]).frame();
            }
            var value = expression.executeGeneric(frame);
            return frameSetter.execute(f, value, frameSlot);
        } else {
            if (valueReference == null) {
                CompilerDirectives.transferToInterpreterAndInvalidate();
                valueReference = ISLISPContext.get(this).lookupGlobalVar(module, name);
            }
            if (valueReference.isReadOnly()) {
                return errorSignalerNode.signalImmutableBindingError(name);
            }
            var value = expression.executeGeneric(frame);
            valueReference.setValue(value);
            return value;
        }
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        if (tag == StandardTags.WriteVariableTag.class) {
            return true;
        }
        return super.hasTag(tag);
    }

}
