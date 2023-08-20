package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.staticobject.DefaultStaticObjectFactory;
import com.oracle.truffle.api.staticobject.StaticProperty;
import com.oracle.truffle.api.staticobject.StaticShape;

import java.util.List;
import java.util.Map;

public record StandardClass(
        List<LispClass> parents,
        StaticShape<DefaultStaticObjectFactory> shape,
        Map<String, StaticProperty> slots
) implements LispClass {

    @Override
    public SourceSection sourceSection() {
        return null;
    }

    @Override
    public List<LispClass> getParents() {
        return parents;
    }

}
