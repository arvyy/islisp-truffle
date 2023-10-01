package com.github.arvyy.islisp;

import java.util.List;

/**
 * Takes complex setf form and returns expanded result
 */
public interface SetfTransformer {

    Object transform(List<Object> form, Object value);

}
