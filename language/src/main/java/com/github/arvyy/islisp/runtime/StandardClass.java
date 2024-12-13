package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.staticobject.DefaultStaticObjectFactory;
import com.oracle.truffle.api.staticobject.StaticProperty;
import com.oracle.truffle.api.staticobject.StaticShape;

import java.util.List;

/**
 * Represents class created through defclass.
 *
 * @param name class name
 * @param parents parent classes
 * @param shape truffle shape to create instances from
 * @param slots defclass slots
 * @param isAbstract abstract flag
 * @param sourceSection associated source section
 */
public record StandardClass(
        String name,
        LispClass[] parents,
        StaticShape<DefaultStaticObjectFactory> shape,
        Slot[] slots,
        boolean isAbstract,
        SourceSection sourceSection
) implements LispClass, TruffleObject {

    /**
     * Standard class' slot information.
     *
     * @param name slot name
     * @param property associated truffle property for manipulation
     * @param initForm initialization form
     * @param initArg initialization argument
     */
    public record Slot(
            Symbol name,
            StaticProperty property,
            LispFunction initForm,
            Symbol initArg
    ) { }

    @Override
    public List<LispClass> getParents() {
        return List.of(parents);
    }

    @Override
    public SourceSection getSourceLocation() {
        return sourceSection;
    }
}

