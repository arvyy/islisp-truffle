package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.parser.ParsingException;
import com.github.arvyy.islisp.runtime.LispClass;
import com.github.arvyy.islisp.runtime.Symbol;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

import java.util.Objects;

/**
 * Implements `defmethod` syntax for creating new method instance for a given generic function.
 */
public class ISLISPDefMethodNode extends ISLISPExpressionNode {

    /**
     * Type of defmethod.
     */
    public enum MethodQualifier {
        none, before, after, around
    }

    private final MethodQualifier methodQualifier;

    private final String module;
    private final Symbol name;

    private final Symbol[] argsClassNames;

    private final boolean hasRest;
    private final boolean setf;

    private RootNode functionNode;

    /**
     * Create defmethod node.
     *
     * @param module module name whose source's this node is part of
     * @param methodQualifier method qualifier
     * @param name generic function's name
     * @param setf is the signature plain or using setf
     * @param argsClassNames parameters' types to use in resolution
     * @param hasRest does method have :rest argument
     * @param functionNode method's body implementation node
     * @param sourceSection relevant source section.
     */
    public ISLISPDefMethodNode(
            String module,
            MethodQualifier methodQualifier,
            Symbol name,
            boolean setf,
            Symbol[] argsClassNames,
            boolean hasRest,
            RootNode functionNode,
            SourceSection sourceSection
    ) {
        super(true, sourceSection);
        this.module = module;
        this.methodQualifier = Objects.requireNonNull(methodQualifier);
        this.name = name;
        this.argsClassNames = argsClassNames;
        this.hasRest = hasRest;
        this.functionNode = functionNode;
        this.setf = setf;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        var ctx = ISLISPContext.get(this);
        var genericFunctionDescriptor = ctx.lookupGenericFunctionDispatchTree(
            module, name.identityReference(), setf);
        if (argsClassNames.length != genericFunctionDescriptor.getRequiredArgCount()) {
            throw new ParsingException(getSourceSection(),
                "defmethod signature doesn't match defgeneric signature");
        }
        if (hasRest != genericFunctionDescriptor.hasRest()) {
            throw new ParsingException(getSourceSection(),
                "defmethod signature doesn't match defgeneric signature");
        }
        var classes = new LispClass[argsClassNames.length];
        for (int i = 0; i < argsClassNames.length; i++) {
            classes[i] = ctx.lookupClass(
                module, argsClassNames[i].identityReference());
        }
        var callTarget = functionNode.getCallTarget();
        switch (methodQualifier) {
            case none -> genericFunctionDescriptor.addPrimaryMethod(classes, callTarget, this);
            case before -> genericFunctionDescriptor.addBeforeMethod(classes, callTarget, this);
            case around -> genericFunctionDescriptor.addAroundMethod(classes, callTarget, this);
            case after -> genericFunctionDescriptor.addAfterMethod(classes, callTarget, this);
            default -> { }
        }
        return name;
    }

}
