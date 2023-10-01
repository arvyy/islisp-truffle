package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.dsl.TypeSystem;

@TypeSystem({
    LispClass.class,
    LispFunction.class,
    Pair.class,
    StandardClassObject.class,
    Symbol.class,
    LispOutputStream.class,
    LispChar.class,
    String.class,
    StringBuffer.class,
    int.class
})
public class ISLISPTypes {
}
