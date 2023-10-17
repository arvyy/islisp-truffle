package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.StandardClassObject;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

public class ISLISPInitializeObject extends RootNode {

    protected ISLISPInitializeObject(TruffleLanguage<?> language) {
        super(language);
    }

    @Override
    public Object execute(VirtualFrame frame) {
        var obj = (StandardClassObject) frame.getArguments()[1];
        var clazz = obj.clazz();
        // TODO optimize
        for (int i = 2; i < frame.getArguments().length; i += 2) {
            var initArg = (Symbol) frame.getArguments()[i];
            for (int j = 0; j < clazz.slots().length; j++) {
                var slot = clazz.slots()[j];
                if (slot.initArg() != null
                    && slot.initArg().getId() == initArg.identityReference().getId()
                    && slot.property().getObject(obj.data()) == null
                ) {
                    slot.property().setObject(obj.data(), frame.getArguments()[i + 1]);
                }
            }
        }
        return obj;
    }

    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(new ISLISPInitializeObject(lang).getCallTarget());
    }

}
