package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/**
 * Lisp vector.
 * @param values vector content
 */
@ExportLibrary(InteropLibrary.class)
public record LispVector(Object[] values) implements TruffleObject {

    @ExportMessage
    boolean hasArrayElements() {
        return true;
    }

    @ExportMessage
    Object readArrayElement(long index) throws InvalidArrayIndexException {
        if (index >= values.length) {
            throw InvalidArrayIndexException.create(index);
        }
        return values[(int) index];
    }

    @ExportMessage
    void writeArrayElement(long index, Object value) throws InvalidArrayIndexException {
        if (index >= values.length) {
            throw InvalidArrayIndexException.create(index);
        }
        values[(int) index] = value;
    }

    @ExportMessage
    long getArraySize() {
        return values.length;
    }

    @ExportMessage
    boolean isArrayElementReadable(long index) {
        return true;
    }
    @ExportMessage
    boolean isArrayElementModifiable(long index) {
        return true;
    }

    @ExportMessage
    boolean isArrayElementInsertable(long index) {
        return false;
    }

}
