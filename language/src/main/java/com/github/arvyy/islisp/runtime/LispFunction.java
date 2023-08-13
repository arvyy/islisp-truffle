package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.TruffleObject;

public record LispFunction(MaterializedFrame closure, CallTarget callTarget) implements Value, TruffleObject {

}
