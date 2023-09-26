package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.source.SourceSection;

import java.io.OutputStream;

public record LispOutputStream(OutputStream outputStream) implements Value {

    @Override
    public SourceSection sourceSection() {
        return null;
    }

}
