package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.source.SourceSection;

public record LispChar(int codepoint, SourceSection sourceSection) implements Value {
}
