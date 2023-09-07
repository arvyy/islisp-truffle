package com.github.arvyy.islisp.runtime;

public class GenericFunctionDescriptor {

    private final int requiredArgCount;
    private final boolean hasRest;
    private final GenericDispatchTree dispatchTree;


    public GenericFunctionDescriptor(int requiredArgCount, boolean hasRest) {
        this.requiredArgCount = requiredArgCount;
        this.hasRest = hasRest;
        this.dispatchTree = new GenericDispatchTree();
    }

    public int getRequiredArgCount() {
        return requiredArgCount;
    }

    public boolean hasRest() {
        return hasRest;
    }

    public GenericDispatchTree getDispatchTree() {
        return dispatchTree;
    }
}