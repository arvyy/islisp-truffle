package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.source.SourceSection;

public class ISLISPDebuggerNode extends ISLISPExpressionNode {

    public ISLISPDebuggerNode(SourceSection sourceSection) {
        super(sourceSection);
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        return ISLISPContext.get(this).getNIL();
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        if (tag == DebuggerTags.AlwaysHalt.class)
            return true;
        return super.hasTag(tag);
    }
}
