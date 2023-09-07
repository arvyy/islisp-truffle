package com.github.arvyy.islisp.runtime;

import com.github.arvyy.islisp.ISLISPError;
import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleRuntime;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.utilities.CyclicAssumption;

import java.util.Comparator;

public class GenericDispatchTree {

    private int size;
    private CallTarget callTarget;
    private LispClass clazz;
    private ArraySlice<GenericDispatchTree> children;
    private CyclicAssumption assumption;

    public GenericDispatchTree() {
        size = 0;
        callTarget = null;
        clazz = null;
        children = new ArraySlice<>(new GenericDispatchTree[0]);
        assumption = new CyclicAssumption("Generic method tree unchanged");
    }

    @CompilerDirectives.TruffleBoundary
    public void addMethod(LispClass[] argTypes, CallTarget callTarget, Node node) {
        addMethod(new ArraySlice<>(argTypes), callTarget, node);
        assumption.invalidate("New method added");
    }

    @CompilerDirectives.TruffleBoundary
    public void addMethod(ArraySlice<LispClass> argTypes, CallTarget callTarget, Node node) {
        size++;
        if (argTypes.size() == 0) {
            if (this.callTarget != null) {
                throw new ISLISPError("Duplicate generic implementation", node); //TODO
            }
            this.callTarget = callTarget;
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
                children.get(index).addMethod(argTypes.drop(1), callTarget, node);
            } else {
                var newNode = new GenericDispatchTree();
                newNode.clazz = nextArg;
                newNode.addMethod(argTypes.drop(1), callTarget, node);
                children = children.add(newNode);
                children.sort(Comparator.comparing(tree -> tree.clazz, this::compareClassSpecificities));
            }
        }
    }

    private int compareClassSpecificities(LispClass cls1, LispClass cls2) {
        if (cls1 == cls2)
            return 0;
        if (isSubclassOf(cls1, cls2)) {
            return -1;
        }
        if (isSubclassOf(cls2, cls1)) {
            return 1;
        }
        return cls1.hashCode() - cls2.hashCode();
    }

    private boolean isSubclassOf(LispClass cls1, LispClass cls2) {
        if (cls1 == cls2)
            return true;
        for (var p: cls1.getParents()) {
            if (isSubclassOf(p, cls2))
                return true;
        }
        return false;
    }

    public int size() {
        return  size;
    }

    @CompilerDirectives.TruffleBoundary
    public ArraySlice<CallTarget> getApplicableMethods(LispClass[] argTypes) {
        var result = new CallTarget[size];
        var usedSize = collectApplicatableMethods(new ArraySlice<>(argTypes), result, 0);
        return new ArraySlice<>(result, 0, usedSize);
    }

    int collectApplicatableMethods(ArraySlice<LispClass> argTypes, CallTarget[] result, int resultIndex) {
        if (argTypes.size() == 0) {
            result[resultIndex] = callTarget;
            return resultIndex + 1;
        }
        var nextArg = argTypes.get(0);
        int[] index = new int[] { resultIndex };
        children.forEach(child -> {
            if (isSubclassOf(nextArg, child.clazz)) {
                index[0] = collectApplicatableMethods(argTypes.drop(1), result, index[0]);
            }
        });
        return index[0];
    }

    public Assumption getAssumption() {
        return assumption.getAssumption();
    }
}