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

import java.util.Objects;

/**
 * Function instantiated for defclass slots with :reader option.
 */
public abstract class ISLISPClassSlotReader extends RootNode {

    private final Symbol slot;

    /**
     * Create slot reader root node.
     *
     * @param slot slot's name
     * @param language language reference
     */
    public ISLISPClassSlotReader(Symbol slot, TruffleLanguage<?> language) {
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
        return Objects.requireNonNullElse(property.getObject(clsObject.data()), ISLISPContext.get(this).getNil());
    }

    @Specialization
    Object doUnspecialized(StandardClassObject clsObject) {
        return Objects.requireNonNullElse(
            lookupProperty(clsObject.clazz()).getObject(clsObject.data()),
            ISLISPContext.get(this).getNil());
    }

    @CompilerDirectives.TruffleBoundary
    StaticProperty lookupProperty(StandardClass clazz) {
        for (var classSlot: clazz.slots()) {
            if (classSlot.name().equals(slot)) {
                return classSlot.property();
            }
        }
        return null;
    }

}
