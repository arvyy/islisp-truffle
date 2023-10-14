package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispClass;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public abstract class ISLISPSubclassp extends RootNode {

    protected ISLISPSubclassp(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    abstract Object executeGeneric(Object clazz1, Object clazz2);

    @Specialization(guards = {
        "clazz1 == clazz1Prev",
        "clazz2 == clazz2Prev"
    })
    Object doProper(
        LispClass clazz1,
        LispClass clazz2,
        @Cached("clazz1") LispClass clazz1Prev,
        @Cached("clazz2") LispClass clazz2Prev,
        @Cached("isSubclass(clazz1, clazz2)") Object result
    ) {
        return result;
    }

    @CompilerDirectives.TruffleBoundary
    Object isSubclass(LispClass clazz1, LispClass clazz2) {
        var t = ISLISPContext.get(this).getT();
        var nil = ISLISPContext.get(this).getNil();
        if (isSubclassHelper(clazz1, clazz2)) {
            return t;
        } else {
            return nil;
        }
    }

    boolean isSubclassHelper(LispClass clazz1, LispClass clazz2) {
        if (clazz1 == clazz2) {
            return true;
        }
        for (var parent: clazz1.getParents()) {
            if (isSubclassHelper(parent, clazz2)) {
                return true;
            }
        }
        return false;
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPSubclasspNodeGen.create(lang).getCallTarget());
    }

}
