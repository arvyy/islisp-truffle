package com.github.arvyy.islisp.runtime;

import com.github.arvyy.islisp.exceptions.ISLISPError;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.nodes.Node;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Objects;

/**
 * A tree of all generic function's method instances for specific type (primary, before, after, around).
 * For each generic parameter, it has as many branches as there are different classes
 * used for said parameter in declared methods, with each branch leading to examination of following generic parameter.
 * The methods are automatically sorted by specificity. Given a set of argument types, returned applicable methods' set
 * is sorted from most specific to least specific.
 */
public class GenericDispatchTree {

    private int size;
    private CallTarget callTarget;
    private LispClass clazz;
    private ArrayList<GenericDispatchTree> children;

    /**
     * Create generic dispatch tree.
     */
    public GenericDispatchTree() {
        size = 0;
        callTarget = null;
        clazz = null;
        children = new ArrayList<>(3);
    }

    /**
     * Add new method entry.
     *
     * @param argTypes array of parameter types for the signature
     * @param pCallTarget implementation
     * @param node relevant node to signal error from
     */
    public void addMethod(ArraySlice<LispClass> argTypes, CallTarget pCallTarget, Node node) {
        Objects.requireNonNull(pCallTarget);
        size++;
        if (argTypes.size() == 0) {
            if (this.callTarget != null) {
                throw new ISLISPError("Duplicate generic implementation", node); //TODO
            }
            this.callTarget = pCallTarget;
        } else {
            var index = -1;
            var nextArg = argTypes.get(0);
            for (int i = 0; i < children.size(); i++) {
                if (children.get(i).clazz == nextArg) {
                    index = i;
                    break;
                }
            }
            if (index != -1) {
                children.get(index).addMethod(argTypes.drop(1), pCallTarget, node);
            } else {
                var newNode = new GenericDispatchTree();
                newNode.clazz = nextArg;
                newNode.addMethod(argTypes.drop(1), pCallTarget, node);
                insertNewDispatchTreeBranch(newNode);
            }
        }
    }

    void insertNewDispatchTreeBranch(GenericDispatchTree branch) {
        int index = children.size();
        for (int i = 0; i < children.size(); i++) {
            if (isSubclassOf(branch.clazz, children.get(i).clazz)) {
                index = i;
                break;
            }
        }
        children.add(index, branch);
    }

    private boolean isSubclassOf(LispClass cls1, LispClass cls2) {
        Objects.requireNonNull(cls1);
        Objects.requireNonNull(cls2);
        if (cls1 == cls2) {
            return true;
        }
        for (var p: cls1.getParents()) {
            if (isSubclassOf(p, cls2)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @return count of the tree entries
     */
    public int size() {
        return  size;
    }

    /**
     * Find applicable methods for given type invocation.
     *
     * @param argTypes types for which found methods must be compatible with
     * @return array of applicable call targets
     */
    public ArraySlice<CallTarget> getApplicableMethods(LispClass[] argTypes) {
        var result = new CallTarget[size];
        var usedSize = collectApplicatableMethods(new ArraySlice<>(argTypes), result, 0);
        return new ArraySlice<>(result, 0, usedSize);
    }

    int collectApplicatableMethods(ArraySlice<LispClass> argTypes, CallTarget[] result, int resultIndex) {
        if (argTypes.size() == 0) {
            result[resultIndex] = Objects.requireNonNull(callTarget);
            return resultIndex + 1;
        }
        var nextArg = argTypes.get(0);
        int[] index = new int[] {resultIndex};
        children.forEach(child -> {
            if (isSubclassOf(nextArg, child.clazz)) {
                index[0] = child.collectApplicatableMethods(argTypes.drop(1), result, index[0]);
            }
        });
        return index[0];
    }

}
