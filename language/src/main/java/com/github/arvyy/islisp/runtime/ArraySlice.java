package com.github.arvyy.islisp.runtime;

import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.function.Consumer;

public record ArraySlice<T>(T[] els, int start, int end) {

    public ArraySlice(T[] els) {
        this(els, 0, els.length);
    }

    public ArraySlice<T> drop(int n) {
        return new ArraySlice<>(els, start + n, end);
    }

    public int size() {
        return end - start;
    }

    public T get(int index) {
        return els[index + start];
    }

    public void set(int index, T t) {
        els[index + start] = t;
    }

    public ArraySlice<T> add(T el) {
        var newEls = Arrays.copyOf(els, els.length + 1);
        return new ArraySlice<>(newEls, start, end + 1);
    }

    public void sort(Comparator<T> comparator) {
        sort(this, comparator);
    }

    public void forEach(Consumer<T> consumer) {
        for (int i = start; i < end; i++) {
            consumer.accept(els[i]);
        }
    }

    static <T> void sort(ArraySlice<T> arr, Comparator<T> comparator) {
        if (arr.size() <= 1) return;
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
