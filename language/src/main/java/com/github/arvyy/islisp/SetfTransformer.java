package com.github.arvyy.islisp;

import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.source.SourceSection;

import java.util.List;

/**
 * Takes complex setf form and returns expanded result
 */
public interface SetfTransformer {

    Value transform(List<Value> form, Value value, SourceSection sourceSection);

}
