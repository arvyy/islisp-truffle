package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.staticobject.DefaultStaticObjectFactory;
import com.oracle.truffle.api.staticobject.StaticProperty;
import com.oracle.truffle.api.staticobject.StaticShape;

import java.util.List;

public record StandardClass(
        LispClass[] parents,
        StaticShape<DefaultStaticObjectFactory> shape,
        Slot[] slots,
        boolean isAbstract
) implements LispClass {

    public record Slot(
            SymbolReference name,
            StaticProperty property,
            LispFunction initForm,
            SymbolReference initArg
    ) { }

    @Override
    public List<LispClass> getParents() {
        return List.of(parents);
    }

}

