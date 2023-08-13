package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

public record Symbol(String name) implements Value, TruffleObject {

    public final static Symbol NIL = new Symbol("NIL");

}
