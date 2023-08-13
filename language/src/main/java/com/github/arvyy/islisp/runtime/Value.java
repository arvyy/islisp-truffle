package com.github.arvyy.islisp.runtime;

public sealed interface Value
    permits Pair, LispInteger, Symbol, LispFunction
{}
