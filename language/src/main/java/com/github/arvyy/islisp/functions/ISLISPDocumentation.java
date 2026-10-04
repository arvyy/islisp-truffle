package com.github.arvyy.islisp.functions;

import java.util.HashMap;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.ISLISPModule;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.LispVector;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class ISLISPDocumentation extends RootNode {
    
    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPDocumentation(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        var args = frame.getArguments();
        if (args.length != 2 && args.length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(args.length - 1, 1, 2);
        }
        var ctx = ISLISPContext.get(this);
        if (args.length == 2) {
            if (args[1] instanceof Symbol s) {
                return ctx.getCurrentModule().getDocumentationIncludeImports(s);
            } else {
                return errorSignalerNode.signalWrongType(args[1], ctx.lookupClass("<symbol>"));
            }
        } else {
            if (args[1] instanceof Symbol ns) {
                if (args[2] instanceof Symbol s) {
                    return ctx.getCurrentModule().getDocumentationIncludeImports(ns, s);
                } else {
                    return errorSignalerNode.signalWrongType(args[2], ctx.lookupClass("<symbol>"));
                }
            } else {
                return errorSignalerNode.signalWrongType(args[1], ctx.lookupClass("<symbol>"));
            }
        }
    }
    
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPDocumentation(lang).getCallTarget());
    }
    
}
