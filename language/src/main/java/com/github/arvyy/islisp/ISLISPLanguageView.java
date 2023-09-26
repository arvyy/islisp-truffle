package com.github.arvyy.islisp;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

@ExportLibrary(value = InteropLibrary.class, delegateTo = "delegate")
public class ISLISPLanguageView implements TruffleObject {

    final Object delegate;

    public ISLISPLanguageView(Object delegate) {
        this.delegate = delegate;
    }

    @ExportMessage
    boolean hasLanguage() {
        return true;
    }

    @ExportMessage
    Class<? extends TruffleLanguage<?>> getLanguage() {
        return ISLISPTruffleLanguage.class;
    }

    @ExportMessage
    Object toDisplayString(
            boolean allowSideEffects,
            @CachedLibrary("this.delegate") InteropLibrary dLib
    ) {
        return "foo";
    }

}
