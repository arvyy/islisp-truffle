package com.github.arvyy.islisp.runtime;

import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.parser.SyntaxObject;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.github.arvyy.islisp.ISLISPContext;

import java.util.Iterator;

/**
 * ISLISP cons cell / pair.
 */
@ExportLibrary(InteropLibrary.class)
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

    @Override
    public String toString() {
        return String.format("(%s . %s)", car, cdr);
    }

    @ExportMessage
    boolean hasMembers() {
        return true;
    }

    @ExportMessage
    LispVector getMembers(boolean ignored) {
        return new LispVector(new Object[]{"car", "cdr"});
    }

    @ExportMessage
    boolean isMemberReadable(String m) {
        switch (m) {
            case "car":
            case "cdr":
                return true;
            default:
                return false;
        }
    }

    @ExportMessage
    Object readMember(String m) {
        switch (m) {
            case "car": return car;
            case "cdr": return cdr;
            default: return null;
        }
    }

}

final class PairIterator implements Iterator<Object> {

    PairIterator() { }

    Object next;

    @Override
    public boolean hasNext() {
        Object unwrapped;
        if (next instanceof SyntaxObject so) {
            unwrapped = so.value();
        } else {
            unwrapped = next;
        }
        if (unwrapped instanceof Pair) {
            return true;
        }
        if (unwrapped instanceof Symbol s && s == ISLISPContext.get(null).getNil()) {
            return false;
        }
        throw new Utils.NotAList();
    }

    @Override
    public Object next() {
        Pair nextPair;
        if (next instanceof Pair p) {
            nextPair = p;
        } else if (next instanceof SyntaxObject so && so.value() instanceof Pair p) {
            nextPair = p;
        } else {
            throw new Utils.NotAList();
        }
        var car = nextPair.car();
        next = nextPair.cdr();
        return car;
    }
}
