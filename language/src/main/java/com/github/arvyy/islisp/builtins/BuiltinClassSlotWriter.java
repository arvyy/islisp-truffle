package com.github.arvyy.islisp.builtins;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
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

public abstract class BuiltinClassSlotWriter extends RootNode {

    private final Symbol slot;

    public BuiltinClassSlotWriter(Symbol slot, TruffleLanguage<?> language) {
        super(language);
        this.slot = slot;
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        var obj = frame.getArguments()[1];
        var value = frame.getArguments()[2];
        return executeGeneric(obj, value);
    }

    abstract Object executeGeneric(Object object, Object value);

    @Specialization(guards = "clsObject.clazz() == clazz", limit = "999")
    Object doSpecialized(
            StandardClassObject clsObject,
            Object value,
            @Cached("clsObject.clazz()") StandardClass clazz,
            @Cached("lookupProperty(clazz)") StaticProperty property
    ) {
        property.setObject(clsObject.data(), value);
        return ISLISPContext.get(this).getNil();
    }

    @Specialization
    Object doUnspecialized(StandardClassObject clsObject, Object value) {
        lookupProperty(clsObject.clazz()).setObject(clsObject.data(), value);
        return ISLISPContext.get(this).getNil();
    }

    @CompilerDirectives.TruffleBoundary
    StaticProperty lookupProperty(StandardClass clazz) {
        for (var classSlot: clazz.slots()) {
            if (classSlot.name().equals(slot.identityReference())) {
                return classSlot.property();
            }
        }
        return null;
    }

    @Fallback
    Object doFallback(Object notClass, Object value) {
        throw new ISLISPError("Bad parameter", this);
    }

}
