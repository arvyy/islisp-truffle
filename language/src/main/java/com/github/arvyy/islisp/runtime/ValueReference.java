package com.github.arvyy.islisp.runtime;

/**
 * Value box, used to be able to cache location instead of doing lookup in map every time.
 */
public class ValueReference {

    private Object value;
    private boolean readOnly;

    /**
     * @return current value
     */
    public Object getValue() {
        return value;
    }

    /**
     * Set value.
     *
     * @param v value
     */
    public void setValue(Object v) {
        value = v;
    }

    /**
     * @return true if value is read only
     */
    public boolean isReadOnly() {
        return readOnly;
    }

    /**
     * Mark the value to be read only.
     * @param readOnly
     */
    public void setReadOnly(boolean readOnly) {
        this.readOnly = readOnly;
    }
}
