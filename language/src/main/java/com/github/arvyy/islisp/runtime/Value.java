package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.source.SourceSection;

public sealed interface Value extends TruffleObject
    permits Pair, LispInteger, Symbol, LispFunction
{
    SourceSection sourceSection();

}
