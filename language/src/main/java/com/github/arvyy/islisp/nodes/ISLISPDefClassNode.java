package com.github.arvyy.islisp.nodes;

import com.github.arvyy.islisp.ISLISPContext;
import com.github.arvyy.islisp.functions.ISLISPClassSlotBoundpNodeGen;
import com.github.arvyy.islisp.functions.ISLISPClassSlotReaderNodeGen;
import com.github.arvyy.islisp.functions.ISLISPClassSlotWriterNodeGen;
import com.github.arvyy.islisp.runtime.*;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.staticobject.DefaultStaticProperty;
import com.oracle.truffle.api.staticobject.StaticProperty;
import com.oracle.truffle.api.staticobject.StaticShape;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Implements `defclass` syntax for creating new classes.
 */
public class ISLISPDefClassNode extends ISLISPExpressionNode {

    private final String module;
    private final Symbol name;
    private final List<Symbol> superclassName;
    private final List<SlotDefinition> slots;
    private final boolean isAbstract;

    @Children
    private final ISLISPExpressionNode[] slotDefMethods;

    @Child
    ISLISPErrorSignalerNode errorSignalerNode;

    /**
     * Create defclass node.
     *
     * @param module module name whose source's this node is part of
     * @param language language reference
     * @param name class name
     * @param superclassName names of superclasses; &lt;object&gt; inferred if empty
     * @param slots list of slot definitions
     * @param isAbstract flag if this is abstract and cannot be directly instantiated
     * @param sourceSection corresponding source section to this node
     */
    public ISLISPDefClassNode(
            TruffleLanguage<?> language,
            String module,
            Symbol name,
            List<Symbol> superclassName,
            List<SlotDefinition> slots,
            boolean isAbstract,
            SourceSection sourceSection
    ) {
        super(sourceSection);
        this.module = module;
        this.name = name;
        this.superclassName = superclassName.isEmpty()
            ? List.of(ISLISPContext.get(this).namedSymbol("<object>"))
            : superclassName;
        this.slots = slots;
        this.isAbstract = isAbstract;
        slotDefMethods = buildSlotFunctionNodes(language);
        errorSignalerNode = new ISLISPErrorSignalerNode(this);
    }

    private ISLISPExpressionNode[] buildSlotFunctionNodes(TruffleLanguage<?> language) {
        var exprs = new ArrayList<ISLISPExpressionNode>();
        for (var slot: slots) {
            for (var reader: slot.getReaderName()) {
                exprs.add(new ISLISPDefGenericNode(module, reader, false, 1, false, null));
                exprs.add(new ISLISPDefMethodNode(
                        module,
                        ISLISPDefMethodNode.MethodQualifier.none,
                        reader,
                        false,
                        new Symbol[]{name},
                        false,
                        ISLISPClassSlotReaderNodeGen.create(slot.getName(), language),
                        getSourceSection()));
            }
            for (var writer: slot.getWriterName()) {
                exprs.add(new ISLISPDefGenericNode(module, writer, false, 2, false, null));
                exprs.add(new ISLISPDefMethodNode(
                        module,
                        ISLISPDefMethodNode.MethodQualifier.none,
                        writer,
                        false,
                        new Symbol[]{ISLISPContext.get(this).namedSymbol("<object>"), name},
                        false,
                        ISLISPClassSlotWriterNodeGen.create(slot.getName(), language),
                        getSourceSection()));
            }
            for (var accessor: slot.getAccessorName()) {
                exprs.add(new ISLISPDefGenericNode(module, accessor, false, 1, false, null));
                exprs.add(new ISLISPDefMethodNode(
                    module,
                    ISLISPDefMethodNode.MethodQualifier.none,
                    accessor,
                    false,
                    new Symbol[]{name},
                    false,
                    ISLISPClassSlotReaderNodeGen.create(slot.getName(), language),
                    getSourceSection()));
                exprs.add(new ISLISPDefGenericNode(module, accessor, true, 2, false, null));
                exprs.add(new ISLISPDefMethodNode(
                    module,
                    ISLISPDefMethodNode.MethodQualifier.none,
                    accessor,
                    true,
                    new Symbol[]{ISLISPContext.get(this).namedSymbol("<object>"), name},
                    false,
                    ISLISPClassSlotWriterNodeGen.create(slot.getName(), language),
                    getSourceSection()));
            }
            for (var boundp: slot.getBoundpName()) {
                exprs.add(new ISLISPDefGenericNode(module, boundp, false, 1, false, null));
                exprs.add(new ISLISPDefMethodNode(
                    module,
                    ISLISPDefMethodNode.MethodQualifier.none,
                    boundp,
                    false,
                    new Symbol[]{name},
                    false,
                    ISLISPClassSlotBoundpNodeGen.create(slot.getName(), language),
                    getSourceSection()));
            }
        }
        return exprs.toArray(ISLISPExpressionNode[]::new);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        executeOutsideTruffle();
        for (var function: slotDefMethods) {
            function.executeGeneric(frame);
        }
        return name;
    }

    @CompilerDirectives.TruffleBoundary
    void executeOutsideTruffle() {
        var ctx = ISLISPContext.get(null);
        var myslots = new HashMap<Symbol, StandardClass.Slot>();
        var shapeBuilder = StaticShape.newBuilder(ctx.getLanguage());
        var superclasses = new ArrayList<LispClass>();
        // collect inherited slots from parent classes
        for (var superclass: superclassName) {
            var clazz = ctx.lookupClass(module, superclass);
            if (clazz == null) {
                errorSignalerNode.signalUndefinedClass(superclass);
            }
            superclasses.add(clazz);
            if (clazz instanceof StandardClass standardClass) {
                for (var parentSlot: standardClass.slots()) {
                    if (!myslots.containsKey(parentSlot.name())) {
                        var property = new DefaultStaticProperty(parentSlot.name().identityReference().getId() + "");
                        shapeBuilder.property(property, Object.class, false);
                        var slot = new StandardClass.Slot(
                                parentSlot.name(),
                                property,
                                parentSlot.initForm(),
                                parentSlot.initArg());
                        myslots.put(parentSlot.name(), slot);
                    }
                }
            }
        }
        // add own unique properties
        for (var slot: slots) {
            var slotInParent = myslots.get(slot.getName());
            StaticProperty property;
            Symbol initArg;
            LispFunction initForm;
            if (slotInParent == null) {
                property = new DefaultStaticProperty(slot.getName().identityReference().getId() + "");
                shapeBuilder.property(property, Object.class, false);
            } else {
                property = slotInParent.property();
            }
            if (slot.getInitArg() != null) {
                initArg = slot.getInitArg();
            } else if (slotInParent != null) {
                initArg = slotInParent.initArg();
            } else {
                initArg = null;
            }
            if (slot.getInitializer() != null) {
                initForm = new LispFunction(slot.getInitializer().getCallTarget());
            } else if (slotInParent != null) {
                initForm = slotInParent.initForm();
            } else {
                initForm = null;
            }
            var newSlot = new StandardClass.Slot(slot.getName(), property, initForm, initArg);
            myslots.put(slot.getName(), newSlot);
        }
        var newClass = new StandardClass(
                name.name(),
                superclasses.toArray(LispClass[]::new),
                shapeBuilder.build(),
                myslots.values().toArray(StandardClass.Slot[]::new),
                isAbstract,
                getSourceSection()
        );
        ctx.registerClass(module, name, newClass);
    }

    @Override
    public boolean isDefinitionNode() {
        return true;
    }

    /**
     * Specifies information about defined slot.
     */
    public static class SlotDefinition {
        private Symbol name;
        private Symbol[] readerName;
        private Symbol[] writerName;
        private Symbol[] accessorName;
        private Symbol[] boundpName;
        private ISLISPRootNode initializer;
        private Symbol initArg;

        Symbol getName() {
            return name;
        }

        /**
         * Set slot name.
         *
         * @param name
         */
        public void setName(Symbol name) {
            this.name = name;
        }

        Symbol[] getReaderName() {
            return readerName;
        }

        /**
         * Set reader function name.
         *
         * @param readerName
         */
        public void setReaderName(Symbol[] readerName) {
            this.readerName = readerName;
        }

        Symbol[] getWriterName() {
            return writerName;
        }

        /**
         * Set writer function name.
         *
         * @param writerName
         */
        public void setWriterName(Symbol[] writerName) {
            this.writerName = writerName;
        }

        Symbol[] getAccessorName() {
            return accessorName;
        }

        /**
         * Set accessor (reader + setf writer) name.
         *
         * @param accessorName
         */
        public void setAccessorName(Symbol[] accessorName) {
            this.accessorName = accessorName;
        }

        Symbol[] getBoundpName() {
            return boundpName;
        }

        /**
         * Set function name for checking if slot is bound.
         *
         * @param boundpName
         */
        public void setBoundpName(Symbol[] boundpName) {
            this.boundpName = boundpName;
        }

        ISLISPRootNode getInitializer() {
            return initializer;
        }

        /**
         * Set initializer node for setting initial value.
         *
         * @param initializer
         */
        public void setInitializer(ISLISPRootNode initializer) {
            this.initializer = initializer;
        }

        Symbol getInitArg() {
            return initArg;
        }

        /**
         * Set argument name to be associated with this slot during creation.
         *
         * @param initArg
         */
        public void setInitArg(Symbol initArg) {
            this.initArg = initArg;
        }
    }

}
