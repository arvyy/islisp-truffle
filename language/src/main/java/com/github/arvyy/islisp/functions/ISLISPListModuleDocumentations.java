package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.ISLISPModule;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispVector;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class ISLISPListModuleDocumentations extends RootNode {
    
    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPListModuleDocumentations(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2) {
            return errorSignalerNode.signalWrongArgumentCount(
                frame.getArguments().length - 1,
                1,
                1);
        }
        var arg = frame.getArguments()[1];
        var c = ISLISPContext.get(this);
        if (arg instanceof ISLISPModule module) {
            return getDocumentation(module);
        } else {
            return errorSignalerNode.signalWrongType(
                arg,
                c.lookupClass("string"));
        }
    }

    @TruffleBoundary 
    Object getDocumentation(ISLISPModule module) {
        var docs = module.getModuleDocumentations();
        var content = docs.stream()
            .map(d -> new LispVector(new Object[]{ d.namespace(), d.symbol(), d.doc()}))
            .toArray();
        return new LispVector(content);
    }
    
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPListModuleDocumentations(lang).getCallTarget());
    }
    
}
