package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.source.SourceSection;

import java.util.Iterator;

@ExportLibrary(InteropLibrary.class)
public final class Pair implements Value, TruffleObject, Iterable<Value> {

    private Value car, cdr;
    private SourceSection sourceSection;

    public Pair(Value car, Value cdr, SourceSection sourceSection) {
        this.car = car;
        this.cdr = cdr;
        this.sourceSection = sourceSection;
    }

    public Value car() {
        return car;
    }

    public void setCar(Value v) {
        car = v;
    }

    public Value cdr() {
        return cdr;
    }

    public void setCdr(Value v) {
        cdr = v;
    }

    public SourceSection sourceSection() {
        return sourceSection;
    }

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
        next = ((Pair) next).cdr();
        return car;
    }
}