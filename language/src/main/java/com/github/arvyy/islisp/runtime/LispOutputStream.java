package com.github.arvyy.islisp.runtime;

import java.io.OutputStream;

/**
 * ISLISP output stream object.
 *
 * @param outputStream
 */
public record LispOutputStream(OutputStream outputStream) { }
