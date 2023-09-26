package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.source.SourceSection;

public record LispInteger(int value, SourceSection sourceSection) implements Value, TruffleObject { }
