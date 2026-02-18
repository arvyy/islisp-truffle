package com.github.arvyy.islisp;

import com.github.arvyy.islisp.parser.SyntaxObject;

import java.util.List;

/**
 * Takes complex setf form and returns expanded result.
 */
public interface SetfTransformer {

    /**
     * Transform `(setf (transformer form ...) value)` (where transformer is bound to this transformer)
     * into a non-setf expression.
     *
     * @param form `form ...` part expressions collected into list
     * @param value value part expression
     * @return new sexpr
     */
    SyntaxObject transform(List<SyntaxObject> form, SyntaxObject value);

}
