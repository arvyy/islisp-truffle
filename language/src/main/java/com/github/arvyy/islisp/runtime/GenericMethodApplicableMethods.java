package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.CallTarget;

public record GenericMethodApplicableMethods(
        ArraySlice<CallTarget> primaryMethods,
        ArraySlice<CallTarget> aroundMethods,
        ArraySlice<CallTarget> beforeMethods,
        ArraySlice<CallTarget> afterMethods
) {
}
