package com.github.arvyy.islisp.runtime;

public record HandlerChain(LispFunction handler, HandlerChain rest) { }
