package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.Utils;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.ISLISPModule;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class ISLISPListModuleDeclarations extends RootNode {
    
    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    ISLISPListModuleDeclarations(TruffleLanguage<?> language) {
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
            return getExportedFunctions(module);
        } else {
            return errorSignalerNode.signalWrongType(
                arg,
                c.lookupClass("string"));
        }
    }

    @TruffleBoundary 
    Object getExportedFunctions(ISLISPModule module) {
        var declarations = module.listExportedFunctions();
        return Utils.listToValue(declarations);
    }
    
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPListModuleDeclarations(lang).getCallTarget());
    }
    
}
