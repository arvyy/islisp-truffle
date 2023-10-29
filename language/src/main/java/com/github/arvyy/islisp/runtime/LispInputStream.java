package com.github.arvyy.islisp.runtime;

import java.io.InputStream;

/**
 * ISLISP input stream object.
 *
 * @param inputStream
 */
public record LispInputStream(InputStream inputStream) { }
