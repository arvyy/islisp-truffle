package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.exceptions.ISLISPUncaughtConditionException;
import com.github.arvyy.islisp.functions.ISLISPFormatObject;
import com.github.arvyy.islisp.functions.ISLISPSignalCondition;
import com.github.arvyy.islisp.parser.Parser;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * Interactive debugging node, started by default condition handler.
 */
public class ISLISPInteractiveDebugger extends Node {

    @Child
    DirectCallNode continueCondition;

    /**
     * Start debugging repl to handle given condition.
     * @param condition
     */
    @CompilerDirectives.TruffleBoundary
    public void startRepl(Object condition) {
        var e = new RuntimeException();
        var stacktrace = TruffleStackTrace.getStackTrace(e);
        ISLISPExpressionNode signallingNode = null;
        boolean signalConditionTraceFound = false;
        Frame debuggingFrame = null;
        outter:
        for (var i = 0; i < stacktrace.size(); i++) {
            var el = stacktrace.get(i);
            if (signalConditionTraceFound) {
                var n = el.getLocation();
                while (n != null) {
                    if (n instanceof ISLISPExpressionNode) {
                        signallingNode = (ISLISPExpressionNode) n;
                        debuggingFrame = el.getFrame();
                        break outter;
                    }
                    n = n.getParent();
                }
            } else {
                if (el.getTarget().getRootNode() instanceof ISLISPSignalCondition) {
                    signalConditionTraceFound = true;
                }
            }
        }
        if (signallingNode != null && debuggingFrame != null) {
            var conditionLocation = signallingNode.getSourceSection();
            var reader = new BufferedReader(new InputStreamReader(System.in));
            var parser = new Parser();
            System.err.flush();
            System.out.flush();
            System.out.println("Entering interactive condition handler");
            if (conditionLocation != null) {
                System.out.printf("Paused at %s:%d\n\t%s\n",
                    conditionLocation.getSource().getName(),
                    conditionLocation.getStartLine(),
                    conditionLocation.getCharacters());
            }
            System.out.println(",c <expression>\t"
                + "Resume from condition using <expression> as the value from nearest node");
            System.out.println(",q\tAbort to top level REPL");
            System.out.println("<expression>\tEvaluate given expression at the point of condition signalling");
            while (true) {
                try {
                    System.out.print("debug> ");
                    var line = reader.readLine();
                    if (line.startsWith(",c")) {
                        var language = ISLISPContext.get(this).getLanguage();
                        var source = Source
                            .newBuilder("islisp", line.substring(",c".length()), "<debug>")
                            .interactive(true)
                            .cached(false)
                            .build();
                        var inlineEval = parser.createInlineDebuggerEvalNode(language, signallingNode, source);
                        var result = inlineEval.execute(debuggingFrame.materialize());
                        getContinueCondition().call(null, condition, result);
                    } else if (line.startsWith(",q")) {
                        throw new ISLISPUncaughtConditionException(condition);
                    } else {
                        var language = ISLISPContext.get(this).getLanguage();
                        var source = Source
                            .newBuilder("islisp", line, "<debug>")
                            .interactive(true)
                            .cached(false)
                            .build();
                        var inlineEval = parser.createInlineDebuggerEvalNode(language, signallingNode, source);
                        var result = inlineEval.execute(debuggingFrame.materialize());
                        System.out.println(ISLISPFormatObject.format(result, true));
                    }
                } catch (IOException ex) {
                    throw new RuntimeException(ex);
                }
            }
        }
        System.out.println(signallingNode);
        System.out.println(condition);
    }

    @CompilerDirectives.TruffleBoundary
    DirectCallNode getContinueCondition() {
        if (continueCondition == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            var callNode = DirectCallNode.create(
                ctx.lookupFunction("ROOT", ctx.namedSymbol("continue-condition"))
                    .callTarget());
            continueCondition = insert(callNode);
        }
        return continueCondition;
    }
}
