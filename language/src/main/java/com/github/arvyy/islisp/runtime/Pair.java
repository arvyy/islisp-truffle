package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

import java.util.Iterator;

public final class Pair implements TruffleObject, Iterable<Object> {

    private Object car;
    private Object cdr;

    public Pair(Object car, Object cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    public Object car() {
        return car;
    }

    public void setCar(Object v) {
        car = v;
    }

    public Object cdr() {
        return cdr;
    }

    public void setCdr(Object v) {
        cdr = v;
    }

    @Override
    public Iterator<Object> iterator() {
        var iterator = new PairIterator();
        iterator.next = this;
        return iterator;
    }

}

class PairIterator implements Iterator<Object> {

    Object next;

    @Override
    public boolean hasNext() {
        return next instanceof Pair;
    }

    @Override
    public Object next() {
        var car = ((Pair) next).car();
        next = ((Pair) next).cdr();
        return car;
    }
}
