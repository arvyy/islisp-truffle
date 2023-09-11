package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.ISLISPError;
import com.github.arvyy.islisp.runtime.LispClass;
import com.github.arvyy.islisp.runtime.LispFunction;
import com.github.arvyy.islisp.runtime.Symbol;
import com.github.arvyy.islisp.runtime.Value;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class ISLISPDefMethodNode extends ISLISPExpressionNode {

    public enum MethodQualifier {
        none, before, after, around
    }

    private final MethodQualifier methodQualifier;

    private final Symbol name;

    private final Symbol[] argsClassNames;

    private final int requiredArgCount;
    private final boolean hasRest;
    @Child
    private ISLISPRootNode functionNode;

    public ISLISPDefMethodNode(
            MethodQualifier methodQualifier,
            Symbol name,
            Symbol[] argsClassNames,
            int requiredArgCount,
            boolean hasRest,
            ISLISPRootNode functionNode
    ) {
        super(true, functionNode.getSourceSection());
        this.methodQualifier = Objects.requireNonNull(methodQualifier);
        this.name = name;
        this.argsClassNames = argsClassNames;
        this.requiredArgCount = requiredArgCount;
        this.hasRest = hasRest;
        this.functionNode = functionNode;
    }

    @Override
    public Value executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var genericFunctionDescriptor = ctx.lookupGenericFunctionDispatchTree(name.identityReference());
        if (requiredArgCount != genericFunctionDescriptor.getRequiredArgCount()) {
            throw new ISLISPError("defmethod signature doesn't match defgeneric", this);
        }
        if (hasRest != genericFunctionDescriptor.hasRest()) {
            throw new ISLISPError("defmethod signature doesn't match defgeneric", this);
        }
        var classes = new LispClass[argsClassNames.length];
        for (int i = 0; i < argsClassNames.length; i++) {
            classes[i] = ctx.lookupClass(argsClassNames[i].identityReference());
        }
        var definition = ctx.lookupGenericFunctionDispatchTree(name.identityReference());
        var callTarget = functionNode.getCallTarget();
        switch (methodQualifier) {
            case none -> definition.addPrimaryMethod(classes, callTarget, this);
            case before -> definition.addBeforeMethod(classes, callTarget, this);
            case around -> definition.addAroundMethod(classes, callTarget, this);
            case after -> definition.addAfterMethod(classes, callTarget, this);
        }
        return name;
    }

}
