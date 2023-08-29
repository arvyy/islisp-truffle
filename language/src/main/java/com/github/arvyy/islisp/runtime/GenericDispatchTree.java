package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.CallTarget;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

//TODO refactor subList to using same list with 'from' index tracking
public class GenericDispatchTree {

    private int size;
    private CallTarget callTarget;
    private LispClass clazz;
    private List<GenericDispatchTree> children;

    public GenericDispatchTree() {
        size = 0;
        callTarget = null;
        clazz = null;
        children = new ArrayList<>();
    }

    public void addMethod(List<LispClass> argTypes, CallTarget callTarget) {
        size++;
        if (argTypes.isEmpty()) {
            if (this.callTarget != null) {
                throw new RuntimeException("Duplicate generic implementation"); //TODO
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
                children.get(index).addMethod(argTypes.subList(1, argTypes.size()), callTarget);
            } else {
                var newNode = new GenericDispatchTree();
                newNode.clazz = nextArg;
                newNode.addMethod(argTypes.subList(1, argTypes.size()), callTarget);
                children.add(newNode);
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

    public List<CallTarget> getApplicableMethods(List<LispClass> argTypes) {
        if (argTypes.isEmpty()) return List.of(callTarget);
        var lst = new ArrayList<CallTarget>();
        var nextArg = argTypes.get(0);
        for (var child: children) {
            if (isSubclassOf(nextArg, child.clazz)) {
                lst.addAll(child.getApplicableMethods(argTypes.subList(1, argTypes.size())));
            }
        }
        return lst;
    }

}
