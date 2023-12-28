package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * ISLISP stream object.
 *
 * @param outputStream output stream, may be null
 * @param inputStream input stream, may be null
 */
public record LispStream(
    OutputStream outputStream,
    InputStream inputStream
) implements TruffleObject { }
