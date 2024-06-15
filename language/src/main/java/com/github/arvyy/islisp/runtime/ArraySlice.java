package com.github.arvyy.islisp.runtime;

import java.util.Arrays;
import java.util.function.Consumer;

/**
 * Implements logical array for which it is cheap to
 * pop elements from either side by moving start or end index.
 *
 * @param els backing array
 * @param start start index of the slice
 * @param end ending index of the slice
 * @param <T> element type
 */
public record ArraySlice<T>(T[] els, int start, int end) {

    /**
     * Create array slice from given array.
     *
     * @param els backing array
     */
    public ArraySlice(T[] els) {
        this(els, 0, els.length);
    }

    /**
     * @param n count
     * @return return array slice with first n elements removed
     */
    public ArraySlice<T> drop(int n) {
        return new ArraySlice<>(els, start + n, end);
    }

    /**
     * @return size of the array.
     */
    public int size() {
        return end - start;
    }

    /**
     * Get value at a given index.
     *
     * @param index index
     * @return value
     */
    public T get(int index) {
        return els[index + start];
    }

    /**
     * Set value of element at a given index.
     *
     * @param index index
     * @param t value
     */
    public void set(int index, T t) {
        els[index + start] = t;
    }

    /**
     * Creates new array with an extra element added to the end.
     *
     * @param el new element
     * @return new array
     */
    public ArraySlice<T> add(T el) {
        var newEls = Arrays.copyOf(els, size() + 1);
        System.arraycopy(els, start, newEls, 0, size());
        newEls[size()] = el;
        return new ArraySlice<>(newEls, 0, size() + 1);
    }

    /**
     * Execute consumer for each valid element.
     *
     * @param consumer consumer function
     */
    public void forEach(Consumer<T> consumer) {
        for (int i = start; i < end; i++) {
            consumer.accept(els[i]);
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ArraySlice o) {
            if (end - start != o.end - o.start) {
                return false;
            }
            for (var i = 0; i < end - start; i++) {
                if (!get(i).equals(o.get(i))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    @Override
    public int hashCode() {
        var arr = new Object[size()];
        for (int i = 0; i < size(); i++) {
            arr[i] = get(i);
        }
        return Arrays.hashCode(arr);
    }
}
