package com.github.arvyy.islisp.parser;

import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.source.SourceSection;

/**
 * Lisp value + source location.
 *
 * @param value wrapped value
 * @param source source location
 */
public record SyntaxObject(Object value, SourceSection source) {

    /**
     * Recursively strips source information to return plain data.
     * @return value
     */
    public Object syntaxToDatum() {
        return syntaxToDatum(value);
    }

    public static SyntaxObject fromDatum(Object datum, SourceSection source) {
        if (datum instanceof SyntaxObject s) {
            return s;
        }
        if (datum instanceof Pair p) {
            return new SyntaxObject(new Pair(fromDatum(p.car(), source), fromDatum(p.cdr(), source)), source);
        }
        if (datum instanceof LispArray arr) {
            return new SyntaxObject(new LispArray(fromDatumArray(arr.data(), source), arr.dimensions()), source);
        }
        if (datum instanceof LispVector v) {
            var data = new Object[v.values().length];
            for (int i = 0; i < data.length; i++) {
                data[i] = fromDatum(v.values()[i], source);
            }
            return new SyntaxObject(new LispVector(data), source);
        }
        return new SyntaxObject(datum, source);
    }

    private static Object syntaxToDatum(Object value) {
        if (value instanceof SyntaxObject s) {
            return syntaxToDatum(s.value());
        }
        if (value instanceof Pair p) {
            var car = syntaxToDatum(p.car());
            var cdr = syntaxToDatum(p.cdr());
            return new Pair(car, cdr);
        }
        if (value instanceof LispArray arr) {
            return new LispArray(syntaxToDatumArray(arr.data()), arr.dimensions());
        }
        if (value instanceof LispVector v) {
            var data = new Object[v.values().length];
            for (int i = 0; i < data.length; i++) {
                data[i] = syntaxToDatum(v.values()[i]);
            }
            return new LispVector(data);
        }
        return value;
    }

    private static Object[] syntaxToDatumArray(Object[] data) {
        var res = new Object[data.length];
        for (int i = 0; i < data.length; i++) {
            var el = data[i];
            Object el2;
            if (el instanceof Object[] arr) {
                el2 = syntaxToDatumArray(arr);
            } else {
                el2 = syntaxToDatum(el);
            }
            res[i] = el2;
        }
        return res;
    }

    private static Object[] fromDatumArray(Object[] data, SourceSection source) {
        var res = new Object[data.length];
        for (int i = 0; i < data.length; i++) {
            var el = data[i];
            Object el2;
            if (el instanceof Object[] arr) {
                el2 = fromDatumArray(arr, source);
            } else {
                el2 = fromDatum(el, source);
            }
            res[i] = el2;
        }
        return res;
    }


}