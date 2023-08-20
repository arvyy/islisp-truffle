package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.source.SourceSection;

public record StandardClassObject(StandardClass clazz, Object data) implements Value {

    @Override
    public SourceSection sourceSection() {
        return null;
    }

    public LispClass getLispClass() {
        return clazz;
    }
}