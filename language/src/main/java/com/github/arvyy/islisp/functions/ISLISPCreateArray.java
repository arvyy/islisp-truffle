package com.github.arvyy.islisp.functions;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.nodes.ISLISPErrorSignalerNode;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;

import java.util.Arrays;
import java.util.List;

import static com.github.arvyy.islisp.Utils.isNil;

/**
 * Implements `create-array` function.
 */
public abstract class ISLISPCreateArray extends RootNode {

    private final static String NAME = "create-array";
    private final static List<String> ARGS = List.of("dimensions", "[initial-element]");
    private final static String REST_ARG = null;
    private final static String RETURN_TYPE = "<basic-array>";
    private final static String DESC = """
This function creates an array of the given dimensions. The dimensions argument is a list of non-negative integers.
The result is of class general-vector if there is only one dimension, or of class general-array* otherwise.
If initial-element is given, the elements of the new array are initialized with this object, otherwise the initialization is implementation defined.
An error shall be signaled if the requested array cannot be allocated (error-id. cannot-create-array).
An error shall be signaled if dimensions is not a proper list of non-negative integers (error-id. domain-error). initial-element may be any ISLISP object.
    """;
        
    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    @Child
    DirectCallNode createVectorNode;

    @Child
    DirectCallNode lengthNode;

    ISLISPCreateArray(TruffleLanguage<?> language) {
        super(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    DirectCallNode getCreateVectorNode() {
        if (createVectorNode == null) {
            CompilerDirectives.transferToInterpreterAndInvalidate();
            var ctx = ISLISPContext.get(this);
            createVectorNode = this.insert(
                DirectCallNode.create(
                    ctx.lookupFunction("ROOT", ctx.namedSymbol("create-vector"))
                        .callTarget()));
        }
        return createVectorNode;
    }

    DirectCallNode getLengthNode() {
        if (lengthNode == null) {
            var ctx = ISLISPContext.get(this);
            CompilerDirectives.transferToInterpreterAndInvalidate();
            lengthNode = this.insert(
                DirectCallNode.create(
                    ctx.lookupFunction("ROOT", ctx.namedSymbol("length"))
                        .callTarget()));
        }
        return lengthNode;
    }

    @Override
    public final Object execute(VirtualFrame frame) {
        if (frame.getArguments().length != 2 && frame.getArguments().length != 3) {
            return errorSignalerNode.signalWrongArgumentCount(frame.getArguments().length - 1, 1, 2);
        }
        Object initValue;
        if (frame.getArguments().length == 3) {
            initValue = frame.getArguments()[2];
        } else {
            initValue = ISLISPContext.get(this).getNil();
        }
        return executeGeneric(frame.getArguments()[1], initValue);
    }

    abstract Object executeGeneric(Object dimensions, Object initValue);

    @Specialization
    Object executeProper(Pair dimensions, Object initValue) {
        if (isNil(this, dimensions.cdr())) {
            return getCreateVectorNode().call(null, dimensions.car(), initValue);
        } else {
            var dimensionsSize = (int) getLengthNode().call(null, dimensions);
            var dimensionsArr = new Integer[dimensionsSize];
            for (int i = 0; i < dimensionsSize; i++) {
                if (dimensions.car() instanceof Integer integer) {
                    dimensionsArr[i] = integer;
                } else if (dimensions.car() instanceof LispBigInteger biginteger) {
                    dimensionsArr[i] = bigIntValue(biginteger);
                } else {
                    var ctx = ISLISPContext.get(this);
                    return errorSignalerNode.signalWrongType(dimensions.car(), ctx.lookupClass("<integer>"));
                }
                if (dimensionsArr[i] < 0) {
                    var ctx = ISLISPContext.get(this);
                    return errorSignalerNode.signalDomainError(
                        "Given dimensions aren't a list of non-negative integers",
                        dimensions,
                        ctx.lookupClass("<list>"));
                }
                if (dimensions.cdr() instanceof Pair p) {
                    dimensions = p;
                }
            }
            return new LispArray(makeArrayContent(new ArraySlice<>(dimensionsArr), initValue), dimensionsSize);
        }
    }

    @CompilerDirectives.TruffleBoundary
    int bigIntValue(LispBigInteger b) {
        return b.data().intValueExact();
    }

    Object[] makeArrayContent(ArraySlice<Integer> dimensions, Object initValue) {
        var content = new Object[dimensions.get(0)];
        if (dimensions.size() == 1) {
            Arrays.fill(content, initValue);
        } else {
            for (int i = 0; i < content.length; i++) {
                content[i] = makeArrayContent(dimensions.drop(1), initValue);
            }
        }
        return content;
    }

    @Fallback
    Object fallback(Object dimensions, Object initValue) {
        var ctx = ISLISPContext.get(this);
        return errorSignalerNode.signalWrongType(dimensions, ctx.lookupClass("<list>"));
    }

    /**
     * Construct LispFunction using this root node.
     * @param lang truffle language reference
     * @return lisp function
     */
    public static LispFunction makeLispFunction(TruffleLanguage<?> lang) {
        return new LispFunction(
            new LispFunction.Documentation(NAME, ARGS, REST_ARG, RETURN_TYPE, DESC),
            ISLISPCreateArrayNodeGen.create(lang).getCallTarget());
    }
}
