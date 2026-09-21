package com.github.arvyy.islisp.functions;

import java.util.HashMap;

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

public class ISLISPDocumentation extends RootNode {
    
    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPDocumentation(TruffleLanguage<?> language) {
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
        if (arg instanceof LispFunction f) {
            var doc = f.documentation();
            if (doc == null) {
                return c.getNil();
            } else {
                return documentationToAlist(doc);
            }
        } else {
            return c.getNil();
        }
    }

    @TruffleBoundary 
    Object documentationToAlist(LispFunction.Documentation doc) {
        var map = new HashMap<String, Object>();
        if (doc.name() != null) {
            map.put("name", doc.name());
        }
        if (doc.arguments() != null) {
            map.put("named-args", new LispVector(doc.arguments().toArray()));
        }
        if (doc.restArgument() != null) {
            map.put("rest-arg", doc.restArgument());
        }
        if (doc.returnType() != null) {
            map.put("return-type", doc.returnType());
        }
        if (doc.docstring() != null) {
            map.put("docstring", doc.docstring());
        }
        return Utils.mapToAlist(map);
    }
    
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPDocumentation(lang).getCallTarget());
    }
    
}
