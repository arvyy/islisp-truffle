package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispClass;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

import java.io.IOException;

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

    @CompilerDirectives.CompilationFinal
    Symbol sActual, sRequiredMin, sRequiredMax, sMessage, sObject, sExpectedClass;

    @CompilerDirectives.CompilationFinal
    LispClass cArityError, cDomainError;

    Symbol sActual() {
        if (sActual == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            sActual = ISLISPContext.get(this).namedSymbol("actual");
        }
        return sActual;
    }

    Symbol sRequiredMin() {
        if (sRequiredMin == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            sRequiredMin = ISLISPContext.get(this).namedSymbol("required-min");
        }
        return sRequiredMin;
    }

    Symbol sRequiredMax() {
        if (sRequiredMax == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            sRequiredMax = ISLISPContext.get(this).namedSymbol("required-max");
        }
        return sRequiredMax;
    }

    Symbol sMessage() {
        if (sMessage == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            sMessage = ISLISPContext.get(this).namedSymbol("message");
        }
        return sMessage;
    }

    Symbol sObject() {
        if (sObject == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            sObject = ISLISPContext.get(this).namedSymbol("object");
        }
        return sObject;
    }

    Symbol sExpectedClass() {
        if (sExpectedClass == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            sExpectedClass = ISLISPContext.get(this).namedSymbol("expected-class");
        }
        return sExpectedClass;
    }

    LispClass cArityError() {
        if (cArityError == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            cArityError = ctx.lookupClass("ROOT", ctx.namedSymbol("<arity-error>").identityReference());
        }
        return cArityError;
    }

    LispClass cDomainError() {
        if (cDomainError == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            cDomainError = ctx.lookupClass("ROOT", ctx.namedSymbol("<domain-error>").identityReference());
        }
        return cDomainError;
    }


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
            cArityError(),
            sActual(), actual,
            sRequiredMin(), min,
            sRequiredMax(), max
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
        var expected = ISLISPContext.get(this).lookupClass("ROOT", "<stream>");
        return signalDomainError("Not an input stream", obj, expected);
    }

    /**
     * Signal error that a given object isn't output stream created with `create-string-output-stream`.
     *
     * @param obj offending object.
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalNotStringOutputStream(Object obj) {
        var expected = ISLISPContext.get(this).lookupClass("ROOT", "<stream>");
        return signalDomainError(
            "Not an output-stream made with create-string-output-stream",
            obj,
            expected);
    }

    /**
     * Signal domain error.
     *
     * @param message message to show
     * @param obj offending object
     * @param expectedClass expected class
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalDomainError(String message, Object obj, LispClass expectedClass) {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            cDomainError(),
            sMessage(), message,
            sObject(), obj,
            sExpectedClass(), expectedClass
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
            ctx.lookupClass("ROOT", ctx.namedSymbol("<unbound-variable>").identityReference()),
            ctx.namedSymbol("name"), name,
            ctx.namedSymbol("namespace"), ctx.namedSymbol("variable")
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    /**
     * Signal error about undefined function.
     *
     * @param name variable name
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalUndefinedFunction(Symbol name) {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass("ROOT", ctx.namedSymbol("<undefined-function>").identityReference()),
            ctx.namedSymbol("name"), name,
            ctx.namedSymbol("namespace"), ctx.namedSymbol("function")
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    /**
     * Signal error about undefined class.
     *
     * @param name class name
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalUndefinedClass(Symbol name) {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass("ROOT", ctx.namedSymbol("<undefined-entity>").identityReference()),
            ctx.namedSymbol("name"), name,
            ctx.namedSymbol("namespace"), ctx.namedSymbol("class")
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    @CompilerDirectives.TruffleBoundary
    DirectCallNode getSignalCallNode() {
        if (signalCallNode == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var callNode = DirectCallNode.create(
                ctx.lookupFunction("ROOT", ctx.namedSymbol("signal-condition").identityReference())
                    .callTarget());
            signalCallNode = insert(callNode);
        }
        return signalCallNode;
    }

    @CompilerDirectives.TruffleBoundary
    DirectCallNode getCreateCallNode() {
        if (createCallNode == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var callNode = DirectCallNode.create(
                ctx.lookupFunction("ROOT", ctx.namedSymbol("create").identityReference())
                    .callTarget());
            createCallNode = insert(callNode);
        }
        return createCallNode;
    }

    @Override
    public SourceSection getSourceSection() {
        return sourceSection;
    }

    /**
     * Signal end of stream.
     *
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalEndOfStream() {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass("ROOT", ctx.namedSymbol("<end-of-stream>").identityReference())
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    /**
     * Signal division by zero.
     *
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalDivisionByZero() {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass("ROOT", ctx.namedSymbol("<division-by-zero>").identityReference())
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    /**
     * Signal index out of range.
     *
     * @param actual received violating index
     * @param bounds upper (exclusive) bound
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalIndexOutOfRange(int actual, int bounds) {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass("ROOT", ctx.namedSymbol("<index-out-of-range-error>").identityReference()),
            ctx.namedSymbol("bounds"), bounds,
            ctx.namedSymbol("actual"), actual
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    /**
     * Signal no-next-method error.
     *
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalNoNextMethod() {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass("ROOT", ctx.namedSymbol("<no-next-method-error>").identityReference())
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    /**
     * Signal arbitrary truffle interop error.
     *
     * @param interopException exception
     * @return undefined object, value of which shouldn't be relied upon.
     */
    @CompilerDirectives.TruffleBoundary
    public Object signalTruffleInteropError(InteropException interopException) {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass("ROOT", ctx.namedSymbol("<truffle-interop-error>").identityReference()),
            ctx.namedSymbol("message"), interopException.getMessage()
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }


    /**
     * Signal arbitrary IO error.
     *
     * @param exception IOException exception
     * @return undefined object, value of which shouldn't be relied upon.
     */
    @CompilerDirectives.TruffleBoundary
    public Object signalIOError(IOException exception) {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass("ROOT", ctx.namedSymbol("<io-error>").identityReference()),
            ctx.namedSymbol("message"), exception.getMessage()
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    /**
     * Signal unknown conversion in the `(convert) form.
     *
     * @param value instance trying to be converted
     * @param to target class
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalUnknownConversion(Object value, Object to) {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass("ROOT", ctx.namedSymbol("<conversion-error>").identityReference()),
            ctx.namedSymbol("value"), value,
            ctx.namedSymbol("to"), to
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }

    /**
     * Signal attempt to reassign immutable binding.
     *
     * @param bindingName binding that is immutable but was tried to reassign
     * @return undefined object, value of which shouldn't be relied upon.
     */
    public Object signalImmutableBindingError(Symbol bindingName) {
        var ctx = ISLISPContext.get(this);
        var condition = getCreateCallNode().call(
            null,
            ctx.lookupClass("ROOT", ctx.namedSymbol("<immutable-binding-error>").identityReference()),
            ctx.namedSymbol("binding"), bindingName
        );
        return getSignalCallNode().call(null, condition, ctx.getNil());
    }
}
