package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.TruffleObject;

public record LispFunction(Closure closure, CallTarget callTarget, boolean isGeneric) implements TruffleObject {

    public LispFunction(CallTarget callTarget) {
        this(new Closure(null, null, null), callTarget, false);
    }
    public LispFunction(MaterializedFrame frame, CallTarget callTarget) {
        this(new Closure(frame, null, null), callTarget, false);
    }

    public LispFunction(GenericMethodApplicableMethods nextMethods, Object[] args, CallTarget callTarget) {
        this(new Closure(null, nextMethods, args), callTarget, true);
    }
    public LispFunction(Closure closure, CallTarget callTarget) {
        this(closure, callTarget, false);
    }

}
