package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPInteractiveExitException;
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
        this.isInteractive = isInteractive;
        dispatchNode = ISLISPFunctionDispatchNodeGen.create();
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var exitFunction = ctx.lookupFunction(
            "ROOT", ctx.namedSymbol("exit"));
        var reportConditionFunction = ctx.lookupFunction(
            "ROOT", ctx.namedSymbol("report-condition"));
        var errorOutputFunction = ctx.lookupFunction(
            "ROOT", ctx.namedSymbol("error-output"));
        var errorOutput = dispatchNode.executeDispatch(errorOutputFunction, new Object[]{});
        var condition = frame.getArguments()[1];
        dispatchNode.executeDispatch(reportConditionFunction, new Object[]{condition, errorOutput});
        if (isInteractive) {
            throw new ISLISPInteractiveExitException(condition);
        } else {
            return dispatchNode.executeDispatch(exitFunction, new Object[] {1});
        }
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @param isInteractive if true, do not exit on unexpected exception
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang, boolean isInteractive) {
        return new LispFunction(new ISLISPDefaultHandler(lang, isInteractive).getCallTarget());
    }
}
