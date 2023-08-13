package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

import java.util.Iterator;

public record Pair(Value car, Value cdr) implements Value, TruffleObject, Iterable<Value> {

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