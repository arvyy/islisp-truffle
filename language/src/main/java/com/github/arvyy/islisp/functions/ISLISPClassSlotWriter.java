package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.StandardClass;
import com.github.arvyy.islisp.runtime.StandardClassObject;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.staticobject.StaticProperty;

/**
 * Function instantiated for defclass slots with :writer option.
 */
public abstract class ISLISPClassSlotWriter extends RootNode {

    private final Symbol slot;

    /**
     * Create slot writer root node.
     *
     * @param slot slot's name
     * @param language language reference
     */
    public ISLISPClassSlotWriter(Symbol slot, TruffleLanguage<?> language) {
        super(language);
        this.slot = slot;
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        var value = frame.getArguments()[1];
        var obj = frame.getArguments()[2];
        return executeGeneric(value, obj);
    }

    abstract Object executeGeneric(Object value, Object classObject);

    @Specialization(guards = "clsObject.clazz() == clazz", limit = "999")
    Object doSpecialized(
            Object value,
            StandardClassObject clsObject,
            @Cached("clsObject.clazz()") StandardClass clazz,
            @Cached("lookupProperty(clazz)") StaticProperty property
    ) {
        property.setObject(clsObject.data(), value);
        return ISLISPContext.get(this).getNil();
    }

    @Specialization
    Object doUnspecialized(Object value, StandardClassObject clsObject) {
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

}
