package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

public record LispInteger(int value) implements Value, TruffleObject {
}
