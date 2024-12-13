package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.LispClass;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `instancep` predicate, returning if a given object belongs to given class.
 */
public abstract class ISLISPInstancep extends RootNode {

    ISLISPInstancep(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        return executeGeneric(frame.getArguments()[1], frame.getArguments()[2]);
    }

    abstract Object executeGeneric(Object obj, Object clazz);

    @Specialization
    Object doProper(
        Object obj,
        LispClass clazz,
        @Cached("create(getClassOfCallTarget())") DirectCallNode classOf,
        @Cached("create(getSubclasspCallTarget())") DirectCallNode subclassp
    ) {
        var objClazz = classOf.call(null, obj);
        return subclassp.call(null, objClazz, clazz);
    }

    CallTarget getClassOfCallTarget() {
        var ctx =  ISLISPContext.get(this);
        return ctx.lookupFunction("ROOT", ctx.namedSymbol("class-of")).callTarget();
    }

    CallTarget getSubclasspCallTarget() {
        var ctx =  ISLISPContext.get(this);
        return ctx.lookupFunction("ROOT", ctx.namedSymbol("subclassp")).callTarget();
    }

    /**
     * Construct LispFunction using this root node.
     *
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(ISLISPInstancepNodeGen.create(lang).getCallTarget());
    }

}
