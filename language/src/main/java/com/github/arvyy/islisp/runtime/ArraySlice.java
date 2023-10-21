package com.github.arvyy.islisp.runtime;

import java.util.Arrays;
import java.util.Comparator;
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

    static <T> void sort(ArraySlice<T> arr, Comparator<T> comparator) {
        if (arr.size() <= 1) {
            return;
        }
        int middle = (arr.end - arr.start) / 2;
        var left = new ArraySlice<>(arr.els, arr.start, middle);
        var right = new ArraySlice<>(arr.els, middle, arr.end);
        sort(left, comparator);
        sort(right, comparator);
        var leftOffset = 0;
        var rightOffset = 0;
        var buffer = Arrays.copyOf(arr.els, arr.size());
        for (int i = 0; i < arr.size(); i++) {
            if (leftOffset >= left.size()) {
                buffer[i] = right.get(rightOffset++);
            } else if (rightOffset >= right.size()) {
                buffer[i] = left.get(leftOffset++);
            } else {
                if (comparator.compare(left.get(leftOffset), right.get(rightOffset)) <= 0) {
                    buffer[i] = left.get(leftOffset++);
                } else {
                    buffer[i] = right.get(rightOffset++);
                }
            }
        }
        System.arraycopy(buffer, 0, arr.els, arr.start, buffer.length);
    }

}
