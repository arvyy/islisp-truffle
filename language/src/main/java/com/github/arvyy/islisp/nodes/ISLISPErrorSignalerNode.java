package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispClass;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.Node;

/**
 * Helper class to signal errors from primitive / native forms.
 */
public class ISLISPErrorSignalerNode extends Node {

    DirectCallNode signalCallNode;
    DirectCallNode createCallNode;

    public Object signalWrongArgumentCount(int actual, int min, int max) {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass(ctx.namedSymbol("<arity-error>").identityReference()),
            ctx.namedSymbol("actual"), actual,
            ctx.namedSymbol("required-min"), min,
            ctx.namedSymbol("required-max"), max
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    public Object signalWrongType(Object obj, LispClass expectedClass) {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass(ctx.namedSymbol("<domain-error>").identityReference()),
            ctx.namedSymbol("object"), obj,
            ctx.namedSymbol("expected-class"), expectedClass
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    @CompilerDirectives.TruffleBoundary
    DirectCallNode getSignalCallNode() {
        if (signalCallNode == null) {
            var ctx = ISLISPContext.get(this);
            signalCallNode = DirectCallNode.create(
                ctx.lookupFunction(ctx.namedSymbol("signal-condition").identityReference())
                    .callTarget());
        }
        return signalCallNode;
    }

    @CompilerDirectives.TruffleBoundary
    DirectCallNode getCreateCallNode() {
        if (createCallNode == null) {
            var ctx = ISLISPContext.get(this);
            createCallNode = DirectCallNode.create(
                ctx.lookupFunction(ctx.namedSymbol("create").identityReference())
                    .callTarget());
        }
        return createCallNode;
    }

}
