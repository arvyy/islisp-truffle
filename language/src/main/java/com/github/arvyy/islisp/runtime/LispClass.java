package com.github.arvyy.islisp.runtime;

import java.util.List;

public sealed interface LispClass extends Value permits StandardClass, BuiltinClass {

    List<LispClass> getParents();
    boolean isAbstract();

}
