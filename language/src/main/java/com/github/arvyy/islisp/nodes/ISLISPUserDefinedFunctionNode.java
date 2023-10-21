package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.functions.ISLISPCallNextMethod;
import com.github.arvyy.islisp.functions.ISLISPHasNextMethod;
import com.github.arvyy.islisp.runtime.Closure;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Pair;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.GenerateWrapper;
import com.oracle.truffle.api.instrumentation.ProbeNode;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Implements prolog of a userdefined function; initiates appropriate
 * frame slots from arguments before invoking body node.
 */
@GenerateWrapper
public class ISLISPUserDefinedFunctionNode extends ISLISPExpressionNode {

    @Child
    private ISLISPExpressionNode body;

    @Child
    private ISLISPCallNextMethod callNextMethod;

    @Child
    private ISLISPHasNextMethod hasNextMethod;

    @Child
    private ISLISPErrorSignalerNode errorSignalerNode;

    private final int[] namedArgumentSlots;
    private final int restArgumentsSlot;

    private final int callNextMethodSlot;
    private final int hasNextMethodSlot;

    /**
     * Create user defined function prolog node.
     *
     * @param language language reference
     * @param body the actual body of the function
     * @param namedArgumentSlots slots to be initialized from invocation arguments
     * @param restArgumentsSlot slot to store rest invocation arguments into; -1 if absent
     * @param callNextMethodSlot slot to store `call-next-method` invocation; -1 if not a generic function
     * @param hasNextMethodSlot slot to store `next-method-p` invocation; -1 if not a generic function
     * @param sourceSection corresponding source section to this node
     */
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
        errorSignalerNode = new ISLISPErrorSignalerNode();
        this.body = body;
        this.namedArgumentSlots = namedArgumentSlots;
        this.restArgumentsSlot = restArgumentsSlot;
        this.callNextMethodSlot = callNextMethodSlot;
        this.hasNextMethodSlot = hasNextMethodSlot;
        body.markRootBody();
        if (hasNextMethodSlot >= 0) {
            hasNextMethod = new ISLISPHasNextMethod(language);
        }
        if (callNextMethodSlot >= 0) {
            callNextMethod = new ISLISPCallNextMethod(language);
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
    public Object executeGeneric(VirtualFrame frame) {
        var argCount = frame.getArguments().length - 1;
        var hasRest = restArgumentsSlot >= 0;
        if (argCount < namedArgumentSlots.length || (argCount > namedArgumentSlots.length && !hasRest)) {
            return errorSignalerNode.signalWrongArgumentCount(
                argCount,
                namedArgumentSlots.length,
                hasRest ? -1 : namedArgumentSlots.length);
        }
        if (callNextMethodSlot >= 0) {
            var closure = (Closure) frame.getArguments()[0];
            frame.setObject(callNextMethodSlot, new LispFunction(closure, callNextMethod.getCallTarget()));
        }
        if (hasNextMethodSlot >= 0) {
            var closure = (Closure) frame.getArguments()[0];
            frame.setObject(hasNextMethodSlot, new LispFunction(closure, hasNextMethod.getCallTarget()));
        }
        for (var i = 0; i < namedArgumentSlots.length; i++) {
            int slot = namedArgumentSlots[i];
            var arg = frame.getArguments()[i + 1];
            frame.setObject(slot, arg);
        }
        if (hasRest) {
            Object value = ISLISPContext.get(this).getNil();
            for (int i = frame.getArguments().length - 1; i >= namedArgumentSlots.length + 1; i--) {
                value = new Pair(frame.getArguments()[i], value);
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
