package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.CallTarget;

/**
 * Specifies active applicable methods for specific invocation of a generic method.
 *
 * @param primaryMethods
 * @param aroundMethods
 * @param beforeMethods
 * @param afterMethods
 */
public record GenericMethodApplicableMethods(
        ArraySlice<CallTarget> primaryMethods,
        ArraySlice<CallTarget> aroundMethods,
        ArraySlice<CallTarget> beforeMethods,
        ArraySlice<CallTarget> afterMethods
) {
}
