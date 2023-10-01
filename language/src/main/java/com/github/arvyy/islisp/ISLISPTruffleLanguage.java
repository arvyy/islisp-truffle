package com.github.arvyy.islisp;

import com.github.arvyy.islisp.parser.EqWrapper;
import com.github.arvyy.islisp.parser.Parser;
import com.github.arvyy.islisp.parser.Reader;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.instrumentation.ProvidedTags;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.source.SourceSection;

import java.util.HashMap;

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
        var sourceMap = new HashMap<EqWrapper, SourceSection>();
        var islispReader = new Reader(request.getSource(), sourceMap);
        var content = islispReader.readAll();
        var parser = new Parser(sourceMap);
        var rootNode = parser.parseRootNode(this, content);
        return rootNode.getCallTarget();
    }

}
