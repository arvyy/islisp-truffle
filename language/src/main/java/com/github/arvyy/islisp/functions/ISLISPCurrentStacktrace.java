package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispVector;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.TruffleStackTraceElement;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

import java.util.ArrayList;
import java.util.List;

/**
 * Get current stacktrace; used as a default method implementation for
 * fill-stacktrace for condition.
 */
public class ISLISPCurrentStacktrace extends RootNode {

    ISLISPCurrentStacktrace(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        return currentStacktrace();
    }

    /**
     * @return current ISLISP stacktrace as a vector of strings.
     */
    public static LispVector currentStacktrace() {
        var e = new RuntimeException();
        var stacktrace = TruffleStackTrace.getStackTrace(e);
        return new LispVector(stacktraceLines(stacktrace));
    }

    @CompilerDirectives.TruffleBoundary
    static Object[] stacktraceLines(List<TruffleStackTraceElement> stackTraceElementList) {
        var lst = new ArrayList<Object>();
        for (var e: stackTraceElementList) {
            var source = findSourceSection(e.getLocation());
            if (source != null)  {
                lst.add(String.format("\tat %s:%d", source.getSource().getName(), source.getStartLine()));
            }
        }
        return lst.toArray();
    }

    static SourceSection findSourceSection(Node n) {
        if (n == null) {
            return null;
        }
        if (n.getSourceSection() != null) {
            return n.getSourceSection();
        }
        return findSourceSection(n.getParent());
    }


    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPCurrentStacktrace(lang).getCallTarget());
    }
}
