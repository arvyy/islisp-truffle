package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

import java.math.BigInteger;

/**
 * Wrapper class around BigInteger for the sake of implement TruffleObject
 * for the required interop.
 *
 * @param data wrapped java big integer
 */
@ExportLibrary(InteropLibrary.class)
public record LispBigInteger(BigInteger data) implements TruffleObject {

    /**
     * Create big int from given long.
     *
     * @param value long representation
     * @return big int
     */
    public static LispBigInteger valueOf(long value) {
        return new LispBigInteger(BigInteger.valueOf(value));
    }

    /**
     * Numeric addition of this node to the other.
     *
     * @param other number
     * @return sum value
     */
    public LispBigInteger add(LispBigInteger other) {
        return new LispBigInteger(data.add(other.data));
    }

    /**
     * Numeric multiplication of this node to other.
     *
     * @param other number
     * @return sum value
     */
    public LispBigInteger multiply(LispBigInteger other) {
        return new LispBigInteger(data.multiply(other.data));
    }

    @ExportMessage
    boolean isNumber() {
        return true;
    }

    @ExportMessage
    boolean fitsInByte() {
        return false;
    }

    @ExportMessage
    boolean fitsInShort() {
        return false;
    }

    @ExportMessage
    boolean fitsInFloat() {
        return false;
    }

    @ExportMessage
    byte asByte() throws UnsupportedMessageException {
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    short asShort() throws UnsupportedMessageException {
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    float asFloat() throws UnsupportedMessageException {
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    boolean fitsInInt() {
        return false;
    }

    @ExportMessage
    boolean fitsInLong() {
        return false;
    }

    @ExportMessage
    boolean fitsInBigInteger() {
        return false;
    }

    @ExportMessage
    boolean fitsInDouble() {
        return false;
    }

    @ExportMessage
    int asInt() throws UnsupportedMessageException {
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    long asLong() throws UnsupportedMessageException {
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    BigInteger asBigInteger() {
        return data;
    }

    @ExportMessage
    double asDouble() throws UnsupportedMessageException {
        throw UnsupportedMessageException.create();
    }


}
