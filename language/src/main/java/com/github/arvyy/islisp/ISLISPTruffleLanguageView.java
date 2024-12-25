package com.github.arvyy.islisp;

import com.github.arvyy.islisp.functions.ISLISPFormatObject;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/**
 * ISLISP language view.
 */
@ExportLibrary(value = InteropLibrary.class)
public class ISLISPTruffleLanguageView implements TruffleObject {

    private final ISLISPContext context;
    private final Object object;

    /**
     * Create language view.
     *
     * @param context islisp context
     * @param object value being viewed
     */
    public ISLISPTruffleLanguageView(ISLISPContext context, Object object) {
        this.context = context;
        this.object = object;
    }

    @ExportMessage
    Object toDisplayString(boolean ignored) {
        return ISLISPFormatObject.format(object, false);
    }
}
