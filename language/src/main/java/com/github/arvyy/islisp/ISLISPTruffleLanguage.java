package com.github.arvyy.islisp;

import com.github.arvyy.islisp.parser.EqWrapper;
import com.github.arvyy.islisp.parser.Parser;
import com.github.arvyy.islisp.parser.Reader;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Option;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.instrumentation.ProvidedTags;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.graalvm.options.OptionCategory;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;
import org.graalvm.options.OptionStability;

import java.io.InputStreamReader;
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

    @Option(help =
        "Paths joined by system separator, relative to which modules are resolved. " +
        "Defaults to working dir.",
        category = OptionCategory.USER, stability = OptionStability.STABLE)
    public static final OptionKey<String> Sourcepath = new OptionKey<>(".");

    @Override
    public ISLISPContext createContext(Env env) {
        return new ISLISPContext(this, env);
    }

    @Override
    public CallTarget parse(ParsingRequest request) {
        var parser = new Parser();
        var rootNode = parser.parseRootNode(this, "MAIN", request.getSource());
        return rootNode.getCallTarget();
    }

    @Override
    protected void initializeContext(ISLISPContext context) throws Exception {
        var preludeSource = Source
            .newBuilder(
                "islisp",
                new InputStreamReader(ISLISPTruffleLanguage.class.getResourceAsStream("/islispprelude.lisp")),
                "islispprelude.lisp")
            .build();
        var parser = new Parser();
        var rootNode = parser.parseRootNode(this, "ROOT", preludeSource);
        rootNode.getCallTarget().call();
        ISLISPContext.get(null).getModule("ROOT").exportAll();
    }

    @Override
    protected Object getLanguageView(ISLISPContext context, Object value) {
        return new ISLISPTruffleLanguageView(context, value);
    }

    @Override
    protected OptionDescriptors getOptionDescriptors() {
        return new ISLISPTruffleLanguageOptionDescriptors();
    }
}
