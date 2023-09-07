package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.SourceSection;

import java.math.BigInteger;

@ExportLibrary(InteropLibrary.class)
public record LispInteger(int value, SourceSection sourceSection) implements Value, TruffleObject {

    @ExportMessage
    public boolean isNumber() {
        return true;
    }

    @ExportMessage
    public int asInt() {
        return value;
    }

    @ExportMessage boolean fitsInByte() { return false; }
    @ExportMessage boolean fitsInShort() { return false; }
    @ExportMessage boolean fitsInInt() { return true; }
    @ExportMessage boolean fitsInLong() { return true; }
    @ExportMessage boolean fitsInFloat() { return true; }
    @ExportMessage boolean fitsInDouble() { return true; }
    @ExportMessage byte asByte() throws UnsupportedMessageException { throw UnsupportedMessageException.create(); }
    @ExportMessage short asShort() throws UnsupportedMessageException { throw UnsupportedMessageException.create(); }
    @ExportMessage long asLong() throws UnsupportedMessageException { return value; }
    @ExportMessage float asFloat() throws UnsupportedMessageException { return value; }
    @ExportMessage double asDouble() throws UnsupportedMessageException { return value; }
    @ExportMessage boolean fitsInBigInteger() { return true; }
    @CompilerDirectives.TruffleBoundary
    @ExportMessage BigInteger asBigInteger() throws UnsupportedMessageException { return new BigInteger("" + value); }

}
