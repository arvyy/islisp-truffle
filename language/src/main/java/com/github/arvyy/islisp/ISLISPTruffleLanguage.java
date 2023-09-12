package com.github.arvyy.islisp;

import com.github.arvyy.islisp.parser.Parser;
import com.github.arvyy.islisp.parser.Reader;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.instrumentation.ProvidedTags;
import com.oracle.truffle.api.instrumentation.StandardTags;

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
        var islispReader = new Reader(request.getSource());
        var content = islispReader.readAll();
        var parser = new Parser();
        var rootNode = parser.parseRootNode(this, content);
        return rootNode.getCallTarget();
    }

    /*
    @Override
    protected Object getLanguageView(ISLISPContext context, Object value) {
        return new ISLISPLanguageView(value);
    }
     */
}