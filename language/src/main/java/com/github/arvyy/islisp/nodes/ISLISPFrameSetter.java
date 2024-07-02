package com.github.arvyy.islisp.nodes;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.nodes.Node;

/**
 * Helper class to set frame slot values with specialization.
 */
@TypeSystemReference(ISLISPTypes.class)
public abstract class ISLISPFrameSetter extends Node {

    /**
     * Set value to a frame for a given slot.
     *
     * @param frame frame
     * @param value value
     * @param slot slot
     * @return value
     */
    public abstract Object execute(Frame frame, Object value, int slot);

    @Specialization(limit = "1")
    int doInt(Frame frame, int value, int slot) {
        frame.setInt(slot, value);
        return value;
    }

    @Specialization(limit = "1")
    double doDouble(Frame frame, double value, int slot) {
        frame.setDouble(slot, value);
        return value;
    }

    @Specialization(limit = "1")
    Object doObject(Frame frame, Object value, int slot) {
        frame.setObject(slot, value);
        return value;
    }

}
