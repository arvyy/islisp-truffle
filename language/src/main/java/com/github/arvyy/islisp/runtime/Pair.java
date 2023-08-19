package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.source.SourceSection;

import java.util.Iterator;

@ExportLibrary(InteropLibrary.class)
public record Pair(Value car, Value cdr, SourceSection sourceSection) implements Value, TruffleObject, Iterable<Value> {

    @Override
    public Iterator<Value> iterator() {
        var iterator = new PairIterator();
        iterator.next = this;
        return iterator;
    }

}

class PairIterator implements Iterator<Value> {

    Value next;

    @Override
    public boolean hasNext() {
        return next instanceof Pair;
    }

    @Override
    public Value next() {
        var car = ((Pair) next).car();
        var cdr = ((Pair) next).cdr();
        next = cdr;
        return car;
    }
}