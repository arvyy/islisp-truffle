package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.MaterializedFrame;

import java.util.List;

// Object passed around as first argument to function invocations. Not all fields can be active at once:
// non-null frame means this is a lambda
// non-null nextMethods & args means this is a generic method
// Since this is passed as first argument, it means closures are linked, ie, frame.getArguments()[0] is also a Closure
public record Closure(MaterializedFrame frame, GenericMethodApplicableMethods applicableMethods, Object[] args) {
}
