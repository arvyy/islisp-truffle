package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class ISLISPSetDocumentation extends RootNode {

    @Child 
    ISLISPErrorSignalerNode errorSignalerNode;

	ISLISPSetDocumentation(TruffleLanguage<?> language) {
		super(language);
		errorSignalerNode = new ISLISPErrorSignalerNode(this);
	}

	@Override
	public Object execute(VirtualFrame frame) {
	    var args = frame.getArguments();
		if (args.length != 4) {
            return errorSignalerNode.signalWrongArgumentCount(args.length - 1, 3, 3);
        }
        var ctx = ISLISPContext.get(this);
        var doc = args[1];
        if (args[2] instanceof Symbol ns) {
            if (args[3] instanceof Symbol s) {
                return ctx.getCurrentModule().setDocumentation(ns, s, doc);
            } else {
                return errorSignalerNode.signalWrongType(args[3], ctx.lookupClass("<symbol>"));
            }
        } else {
            return errorSignalerNode.signalWrongType(args[2], ctx.lookupClass("<symbol>"));
        }
	}
	
	public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPSetDocumentation(lang).getCallTarget());
    }
    
}
