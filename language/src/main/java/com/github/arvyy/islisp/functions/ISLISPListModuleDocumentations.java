package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.runtime.ISLISPModule;
import com.github.arvyy.islisp.runtime.LispVector;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.frame.VirtualFrame;

/**
 * Implements `list-module-documentations` function.
 */
public class ISLISPListModuleDocumentations extends ISLISPBuiltinFunctionRootNode {

    /**
    * Construct node.
    * @param language truffle language reference
    */
    public ISLISPListModuleDocumentations(TruffleLanguage<?> language) {
        super("list-module-documentations", arg(1), language);
    }

    @Override
    public final Object executeImpl(VirtualFrame frame) {
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
            .map(d -> new LispVector(new Object[]{d.namespace(), d.symbol(), d.doc()}))
            .toArray();
        return new LispVector(content);
    }

}
