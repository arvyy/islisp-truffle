package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

import java.io.OutputStream;

/**
 * ISLISP output stream object.
 *
 * @param outputStream
 */
public record LispOutputStream(OutputStream outputStream) implements TruffleObject { }
