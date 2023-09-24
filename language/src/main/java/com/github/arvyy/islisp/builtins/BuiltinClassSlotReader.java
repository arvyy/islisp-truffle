package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.StandardClass;
import com.github.arvyy.islisp.runtime.StandardClassObject;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.staticobject.StaticProperty;

import java.util.Objects;

public abstract class BuiltinClassSlotReader extends RootNode {

    private final Symbol slot;

    public BuiltinClassSlotReader(Symbol slot, TruffleLanguage<?> language) {
        super(language);
        this.slot = slot;
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        var obj = frame.getArguments()[1];
        return executeGeneric(obj);
    }

    abstract Object executeGeneric(Object object);

    @Specialization(guards = "clsObject.clazz() == clazz", limit = "999")
    Object doSpecialized(
            StandardClassObject clsObject,
            @Cached("clsObject.clazz()") StandardClass clazz,
            @Cached("lookupProperty(clazz)") StaticProperty property
    ) {
        return Objects.requireNonNullElse(property.getObject(clsObject.data()), ISLISPContext.get(this).getNIL());
    }

    @Specialization
    Object doUnspecialized(StandardClassObject clsObject) {
        return lookupProperty(clsObject.clazz()).getObject(clsObject.data());
    }

    @CompilerDirectives.TruffleBoundary
    StaticProperty lookupProperty(StandardClass clazz) {
        for (var slot: clazz.slots()) {
            if (slot.name().equals(this.slot.identityReference())) {
                return slot.property();
            }
        }
        return null;
    }

    @Fallback
    Object doFallback(Object o) {
        throw new ISLISPError("Bad parameter", this);
    }

}