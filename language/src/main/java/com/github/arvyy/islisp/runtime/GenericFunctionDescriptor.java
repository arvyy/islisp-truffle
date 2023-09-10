package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.utilities.CyclicAssumption;

public class GenericFunctionDescriptor {

    private final int requiredArgCount;
    private final boolean hasRest;
    private final GenericDispatchTree primaryMethods;
    private final GenericDispatchTree beforeMethods;
    private final GenericDispatchTree aroundMethods;
    private final GenericDispatchTree afterMethods;

    private final CyclicAssumption assumption;


    public GenericFunctionDescriptor(int requiredArgCount, boolean hasRest) {
        this.requiredArgCount = requiredArgCount;
        this.hasRest = hasRest;
        this.primaryMethods = new GenericDispatchTree();
        this.beforeMethods = new GenericDispatchTree();
        this.aroundMethods = new GenericDispatchTree();
        this.afterMethods = new GenericDispatchTree();
        assumption = new CyclicAssumption("Generic method tree unchanged");
    }

    public int getRequiredArgCount() {
        return requiredArgCount;
    }

    public boolean hasRest() {
        return hasRest;
    }

    public Assumption getAssumption() {
        return assumption.getAssumption();
    }

    @CompilerDirectives.TruffleBoundary
    public void addPrimaryMethod(LispClass[] argTypes, CallTarget callTarget, Node node) {
        primaryMethods.addMethod(new ArraySlice<>(argTypes), callTarget, node);
        assumption.invalidate("New method added");
    }

    @CompilerDirectives.TruffleBoundary
    public void addBeforeMethod(LispClass[] argTypes, CallTarget callTarget, Node node) {
        beforeMethods.addMethod(new ArraySlice<>(argTypes), callTarget, node);
        assumption.invalidate("New method added");
    }

    @CompilerDirectives.TruffleBoundary
    public void addAroundMethod(LispClass[] argTypes, CallTarget callTarget, Node node) {
        aroundMethods.addMethod(new ArraySlice<>(argTypes), callTarget, node);
        assumption.invalidate("New method added");
    }

    @CompilerDirectives.TruffleBoundary
    public void addAfterMethod(LispClass[] argTypes, CallTarget callTarget, Node node) {
        afterMethods.addMethod(new ArraySlice<>(argTypes), callTarget, node);
        assumption.invalidate("New method added");
    }

    @CompilerDirectives.TruffleBoundary
    public GenericMethodApplicableMethods getApplicableMethods(LispClass[] argTypes) {
        // after methods need to have reverse specificity
        var after = afterMethods.getApplicableMethods(argTypes);
        for (int i = 0; i < after.els().length / 2; i++) {
            var ii = after.els().length - i - 1;
            var tmp = after.els()[i];
            after.els()[i] = after.els()[ii];
            after.els()[ii] = tmp;
        }
        return new GenericMethodApplicableMethods(
                primaryMethods.getApplicableMethods(argTypes),
                aroundMethods.getApplicableMethods(argTypes),
                beforeMethods.getApplicableMethods(argTypes),
                after
        );
    }


}