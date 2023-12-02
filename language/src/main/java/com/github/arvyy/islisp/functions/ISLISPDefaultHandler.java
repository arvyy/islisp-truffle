package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.github.arvyy.islisp.nodes.ISLISPFunctionDispatchNode;
import com.github.arvyy.islisp.nodes.ISLISPFunctionDispatchNodeGen;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

/**
 * Implements default condition-handler. Equivalent to
 * (lambda (condition)
 *   (report-condition condition (error-output))
 *   (exit 1))
 */
public class ISLISPDefaultHandler extends RootNode {

    @Child
    private ISLISPFunctionDispatchNode dispatchNode;

    private final boolean isInteractive;

    ISLISPDefaultHandler(TruffleLanguage<?> language, boolean isInteractive) {
        super(language);
        dispatchNode = ISLISPFunctionDispatchNodeGen.create();
        this.isInteractive = isInteractive;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var exitFunction = ctx.lookupFunction(ctx.namedSymbol("exit").identityReference());
        var reportConditionFunction = ctx.lookupFunction(ctx.namedSymbol("report-condition").identityReference());
        var errorOutputFunction = ctx.lookupFunction(ctx.namedSymbol("error-output").identityReference());
        var errorOutput = dispatchNode.executeDispatch(errorOutputFunction, new Object[]{});
        dispatchNode.executeDispatch(reportConditionFunction, new Object[]{frame.getArguments()[1], errorOutput});
        if (isInteractive) {
            throw new ISLISPError("Uncaught ISLISP signal", this);
        } else {
            return dispatchNode.executeDispatch(exitFunction, new Object[] {1});
        }
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang, boolean isInteractive) {
        return new LispFunction(new ISLISPDefaultHandler(lang, isInteractive).getCallTarget());
    }
}
