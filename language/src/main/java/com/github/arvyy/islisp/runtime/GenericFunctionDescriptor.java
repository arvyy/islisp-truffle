package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.utilities.CyclicAssumption;

/**
 * Defines generic function data: signature as well as registered method instances.
 */
public class GenericFunctionDescriptor {

    private final int requiredArgCount;
    private final boolean hasRest;
    private final GenericDispatchTree primaryMethods;
    private final GenericDispatchTree beforeMethods;
    private final GenericDispatchTree aroundMethods;
    private final GenericDispatchTree afterMethods;

    private final CyclicAssumption assumption;


    /**
     * Create function descriptor.
     *
     * @param requiredArgCount required argument count
     * @param hasRest if signature has rest parameter
     */
    public GenericFunctionDescriptor(int requiredArgCount, boolean hasRest) {
        this.requiredArgCount = requiredArgCount;
        this.hasRest = hasRest;
        this.primaryMethods = new GenericDispatchTree();
        this.beforeMethods = new GenericDispatchTree();
        this.aroundMethods = new GenericDispatchTree();
        this.afterMethods = new GenericDispatchTree();
        assumption = new CyclicAssumption("Generic method tree unchanged");
    }

    /**
     * @return signature's required arg count.
     */
    public int getRequiredArgCount() {
        return requiredArgCount;
    }

    /**
     * @return if signature has rest parameter.
     */
    public boolean hasRest() {
        return hasRest;
    }

    /**
     * @return assumption to be used that the tree hasn't been changed.
     */
    public Assumption getAssumption() {
        return assumption.getAssumption();
    }

    /**
     * Add primary method.
     *
     * @param argTypes argument types
     * @param callTarget call target
     * @param node node to be used in error reporting
     */
    @CompilerDirectives.TruffleBoundary
    public void addPrimaryMethod(LispClass[] argTypes, CallTarget callTarget, Node node) {
        primaryMethods.addMethod(new ArraySlice<>(argTypes), callTarget, node);
        assumption.invalidate("New method added");
    }

    /**
     * Add before method.
     *
     * @param argTypes argument types
     * @param callTarget call target
     * @param node node to be used in error reporting
     */
    @CompilerDirectives.TruffleBoundary
    public void addBeforeMethod(LispClass[] argTypes, CallTarget callTarget, Node node) {
        beforeMethods.addMethod(new ArraySlice<>(argTypes), callTarget, node);
        assumption.invalidate("New method added");
    }

    /**
     * Add around method.
     *
     * @param argTypes argument types
     * @param callTarget call target
     * @param node node to be used in error reporting
     */
    @CompilerDirectives.TruffleBoundary
    public void addAroundMethod(LispClass[] argTypes, CallTarget callTarget, Node node) {
        aroundMethods.addMethod(new ArraySlice<>(argTypes), callTarget, node);
        assumption.invalidate("New method added");
    }

    /**
     * Add after method.
     *
     * @param argTypes argument types
     * @param callTarget call target
     * @param node node to be used in error reporting
     */
    @CompilerDirectives.TruffleBoundary
    public void addAfterMethod(LispClass[] argTypes, CallTarget callTarget, Node node) {
        afterMethods.addMethod(new ArraySlice<>(argTypes), callTarget, node);
        assumption.invalidate("New method added");
    }

    /**
     * Find applicable methods for given argument types.
     *
     * @param argTypes parameter types used for dispatch
     * @return applicable methods set, sorted in necessary specificity order
     */
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
