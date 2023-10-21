package com.github.arvyy.islisp.runtime;

/**
 * Linked list of condition handlers.
 *
 * @param handler
 * @param rest
 */
public record HandlerChain(LispFunction handler, HandlerChain rest) { }
