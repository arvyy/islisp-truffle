package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.frame.MaterializedFrame;

/**
 * Defines a closure object that is passed around to function invocations.
 * Not all fields can be active at once:
 * non-null frame means this is a lambda
 * non-null nextMethods & args means this is a generic method
 * Since this is passed as first argument, it means closures are linked, ie, frame.getArguments()[0] is also a Closure
 *
 * @param frame materialized closure frame
 * @param applicableMethods active applicable methods information in case of a generic chain call
 * @param args initial arguments to the generic call
 */
public record Closure(MaterializedFrame frame, GenericMethodApplicableMethods applicableMethods, Object[] args) {
}
