package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispClass;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Helper class to signal errors from primitive / native forms.
 */
public class ISLISPErrorSignalerNode extends Node {

    private final SourceSection sourceSection;

    /**
     * Create helper error signaler node.
     *
     * @param source node from which error will be signalled
     */
    public ISLISPErrorSignalerNode(Node source) {
        sourceSection = source.getSourceSection();
    }

    @Child
    DirectCallNode signalCallNode;

    @Child
    DirectCallNode createCallNode;

    /**
     * Signal error about wrong count of supplied arguments.
     *
     * @param actual actual arg count
     * @param min minimum required argument count
     * @param max maximum argument count (or -1 if unbound)
     * @return undefined object, value of which shouldn't be relied upon.
     */
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

    /**
     * Signal error about unexpected object being supplied.
     *
     * @param obj offending object
     * @param expectedClass expected class
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalWrongType(Object obj, LispClass expectedClass) {
        return signalDomainError("Unexpected type", obj, expectedClass);
    }

    /**
     * Signal error that a given object isn't input stream that can be read from.
     *
     * @param obj offending object.
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalNotAnInputStream(Object obj) {
        var expected = ISLISPContext.get(this).lookupClass("<stream>");
        return signalDomainError("Not an input stream", obj, expected);
    }

    /**
     * Signal error that a given object isn't output stream created with `create-string-output-stream`.
     *
     * @param obj offending object.
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalNotStringOutputStream(Object obj) {
        var expected = ISLISPContext.get(this).lookupClass("<stream>");
        return signalDomainError(
            "Not an output-stream made with create-string-output-stream",
            obj,
            expected);
    }

    Object signalDomainError(String message, Object obj, LispClass expectedClass) {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass(ctx.namedSymbol("<domain-error>").identityReference()),
            ctx.namedSymbol("message"), message,
            ctx.namedSymbol("object"), obj,
            ctx.namedSymbol("expected-class"), expectedClass
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    /**
     * Signal error about unbound variable.
     *
     * @param name variable name
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalUnboundVariable(Symbol name) {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass(ctx.namedSymbol("<unbound-variable>").identityReference()),
            ctx.namedSymbol("name"), name
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    @CompilerDirectives.TruffleBoundary
    DirectCallNode getSignalCallNode() {
        if (signalCallNode == null) {
            var ctx = ISLISPContext.get(this);
            var callNode = DirectCallNode.create(
                ctx.lookupFunction(ctx.namedSymbol("signal-condition").identityReference())
                    .callTarget());
            signalCallNode = insert(callNode);
        }
        return signalCallNode;
    }

    @CompilerDirectives.TruffleBoundary
    DirectCallNode getCreateCallNode() {
        if (createCallNode == null) {
            var ctx = ISLISPContext.get(this);
            var callNode = DirectCallNode.create(
                ctx.lookupFunction(ctx.namedSymbol("create").identityReference())
                    .callTarget());
            createCallNode = insert(callNode);
        }
        return createCallNode;
    }

    @Override
    public SourceSection getSourceSection() {
        return sourceSection;
    }
}
