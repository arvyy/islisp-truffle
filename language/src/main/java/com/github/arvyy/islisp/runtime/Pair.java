package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

import java.util.Iterator;

/**
 * ISLISP cons cell / pair.
 */
public final class Pair implements TruffleObject, Iterable<Object> {

    private Object car;
    private Object cdr;

    /**
     * Create pair.
     *
     * @param car first slot value
     * @param cdr second slot value
     */
    public Pair(Object car, Object cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    /**
     * Get first slot value.
     *
     * @return first value
     */
    public Object car() {
        return car;
    }

    /**
     * Set first slot value.
     *
     * @param v new value
     */
    public void setCar(Object v) {
        car = v;
    }

    /**
     * Get second slot value.
     *
     * @return second value
     */
    public Object cdr() {
        return cdr;
    }

    /**
     * Set second slot value.
     *
     * @param v new value
     */
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

final class PairIterator implements Iterator<Object> {

    PairIterator() { }

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
