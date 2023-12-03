package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

import java.io.InputStream;

/**
 * ISLISP input stream object.
 *
 * @param inputStream
 */
public record LispInputStream(InputStream inputStream) implements TruffleObject { }
