package com.github.arvyy.islisp;

import com.github.arvyy.islisp.parser.Parser;
import com.github.arvyy.islisp.parser.Reader;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;

@TruffleLanguage.Registration(id = "islisp", name = "ISLISP")
public class ISLISPTruffleLanguage extends TruffleLanguage<ISLISPContext> {

    @Override
    public ISLISPContext createContext(Env env) {
        return new ISLISPContext(this, env);
    }

    @Override
    public CallTarget parse(ParsingRequest request) throws Exception {
        var reader = request.getSource().getReader();
        var islispReader = new Reader(reader);
        var content = islispReader.readAll();
        var parser = new Parser();
        var rootNode = parser.parseRootNode(this, content);
        return rootNode.getCallTarget();
    }

}