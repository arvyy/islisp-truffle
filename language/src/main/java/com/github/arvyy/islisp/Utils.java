package com.github.arvyy.islisp;

import com.github.arvyy.islisp.runtime.Pair;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.source.SourceSection;

import java.util.ArrayList;
import java.util.List;

public class Utils {
    public static List<Value> readList(Value v) {
        if (v instanceof Pair p) {
            var lst = new ArrayList<Value>();
            for (var el : p) {
                lst.add(el);
            }
            return lst;
        } else if (v instanceof Symbol s) {
            if (s.identityReference() == ISLISPContext.get(null).getNIL().identityReference()) {
                return List.of();
            }
        }
        throw new RuntimeException();
    }

    public static Value listToValue(List<Value> lst, SourceSection sourceSection) {
        Value val = ISLISPContext.get(null).getNIL();
        for (int i = lst.size() - 1; i >= 0; i--) {
            val = new Pair(lst.get(i), val, null);
        }
        if (val instanceof Pair p) {
            return new Pair(p.car(), p.cdr(), sourceSection);
        }
        return val;
    }
}
