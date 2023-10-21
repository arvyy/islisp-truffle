package com.github.arvyy.islisp;

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
    Object transform(List<Object> form, Object value);

}
