package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPFunctionDispatchNode;
import com.github.arvyy.islisp.nodes.ISLISPFunctionDispatchNodeGen;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.StandardClass;
import com.github.arvyy.islisp.runtime.StandardClassObject;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements `create` method for &lt;standard-class&gt;.
 */
public class ISLISPCreateStandardClassObject extends RootNode {

    @Child
    ISLISPFunctionDispatchNode dispatchNode;

    @CompilerDirectives.CompilationFinal
    private LispFunction initializeObjectFunction;

    ISLISPCreateStandardClassObject(TruffleLanguage<?> language) {
        super(language);
        dispatchNode = ISLISPFunctionDispatchNodeGen.create();
    }

    @Override
    public Object execute(VirtualFrame frame) {
        if (initializeObjectFunction == null) {
            var ctx = ISLISPContext.get(this);
            initializeObjectFunction = ctx.lookupFunction(
                "ROOT", ctx.namedSymbol("initialize-object"));
        }
        var clazz = (StandardClass) frame.getArguments()[1];
        var obj = new StandardClassObject(clazz, clazz.shape().getFactory().create());
        var args = new Object[frame.getArguments().length - 1];
        args[0] = obj;
        System.arraycopy(frame.getArguments(), 2, args, 1, args.length - 1);
        return dispatchNode.executeDispatch(initializeObjectFunction, args);
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPCreateStandardClassObject(lang).getCallTarget());
    }

}
