package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.builtins.BuiltinCallNextMethod;
import com.github.arvyy.islisp.builtins.BuiltinHasNextMethod;
import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Pair;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

@GenerateWrapper
public class ISLISPUserDefinedFunctionNode extends ISLISPExpressionNode {

    @Child
    private ISLISPExpressionNode body;

    @Child
    private BuiltinCallNextMethod callNextMethod;

    @Child
    private BuiltinHasNextMethod hasNextMethod;

    private final int[] namedArgumentSlots;
    private final int restArgumentsSlot;

    private final int callNextMethodSlot;
    private final int hasNextMethodSlot;

    public ISLISPUserDefinedFunctionNode(
            TruffleLanguage<?> language,
            ISLISPExpressionNode body,
            int[] namedArgumentSlots,
            int restArgumentsSlot,
            int callNextMethodSlot,
            int hasNextMethodSlot,
            SourceSection sourceSection
    ) {
        super(sourceSection);
        this.body = body;
        this.namedArgumentSlots = namedArgumentSlots;
        this.restArgumentsSlot = restArgumentsSlot;
        this.callNextMethodSlot = callNextMethodSlot;
        this.hasNextMethodSlot = hasNextMethodSlot;
        body.markRootBody();
        if (hasNextMethodSlot >= 0) {
            hasNextMethod = new BuiltinHasNextMethod(language);
        }
        if (callNextMethodSlot >= 0) {
            callNextMethod = new BuiltinCallNextMethod(language);
        }
    }

    protected ISLISPUserDefinedFunctionNode() {
        super(null);
        namedArgumentSlots = null;
        callNextMethodSlot = -1;
        hasNextMethodSlot = -1;
        restArgumentsSlot = -1;
    }

    @Override
    @ExplodeLoop
    public Value executeGeneric(VirtualFrame frame) {
        if (callNextMethodSlot >= 0) {
            var closure = (Closure) frame.getArguments()[0];
            frame.setObject(callNextMethodSlot, new LispFunction(closure, callNextMethod.getCallTarget()));
        }
        if (hasNextMethodSlot >= 0) {
            var closure = (Closure) frame.getArguments()[0];
            frame.setObject(hasNextMethodSlot, new LispFunction(closure, hasNextMethod.getCallTarget()));
        }
        for (var i = 0; i < namedArgumentSlots.length; i++) {
            //TODO validate all arguments supplied
            int slot = namedArgumentSlots[i];
            var arg = frame.getArguments()[i + 1];
            frame.setObject(slot, arg);
        }
        if (restArgumentsSlot >= 0) {
            Value value = ISLISPContext.get(this).getNil();
            for (int i = frame.getArguments().length - 1; i >= namedArgumentSlots.length + 1; i--) {
                value = new Pair((Value) frame.getArguments()[i], value, null);
            }
            frame.setObject(restArgumentsSlot, value);
        }
        return body.executeGeneric(frame);
    }

    @Override
    public boolean isInstrumentable() {
        return true;
    }

    @Override
    public boolean hasTag(Class<? extends Tag> tag) {
        return tag == StandardTags.RootTag.class;
    }

    @Override
    public WrapperNode createWrapper(ProbeNode probe) {
        return new ISLISPUserDefinedFunctionNodeWrapper(this, probe);
    }

}
