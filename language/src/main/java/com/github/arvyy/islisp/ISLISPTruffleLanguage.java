package com.github.arvyy.islisp;

import com.github.arvyy.islisp.parser.EqWrapper;
import com.github.arvyy.islisp.parser.Parser;
import com.github.arvyy.islisp.parser.Reader;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.instrumentation.ProvidedTags;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;

import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Truffle framework entrypoint for ISLISP.
 */
@TruffleLanguage.Registration(id = "islisp", name = "ISLISP")
@ProvidedTags({
    StandardTags.ExpressionTag.class,
    StandardTags.StatementTag.class,
    StandardTags.CallTag.class,
    StandardTags.RootTag.class,
    StandardTags.RootBodyTag.class,
    StandardTags.ReadVariableTag.class,
    StandardTags.WriteVariableTag.class,
    DebuggerTags.AlwaysHalt.class
})
public class ISLISPTruffleLanguage extends TruffleLanguage<ISLISPContext> {

    @Override
    public ISLISPContext createContext(Env env) {
        return new ISLISPContext(this, env);
    }

    @Override
    public CallTarget parse(ParsingRequest request) throws Exception {
        var ctx = ISLISPContext.get(null);
        var sourceMap = new HashMap<EqWrapper, SourceSection>();
        var content = new ArrayList<>();
        if (ctx.isPreludeInitialized()) {
            ctx.setPreludeInitialized();
            var preludeSource = Source
                .newBuilder(
                    "islisp",
                    new InputStreamReader(ISLISPTruffleLanguage.class.getResourceAsStream("/islispprelude.lisp")),
                    "islispprelude.lisp")
                .build();
            var preludeContent = new Reader(preludeSource, sourceMap).readAll();
            content.addAll(preludeContent);
        }
        var userContent = new Reader(request.getSource(), sourceMap).readAll();
        content.addAll(userContent);
        var parser = new Parser(sourceMap);
        var rootNode = parser.parseRootNode(this, content, request.getSource().isInteractive());
        return rootNode.getCallTarget();
    }

    @Override
    protected Object getLanguageView(ISLISPContext context, Object value) {
        return new ISLISPTruffleLanguageView(context, value);
    }
}
