package com.github.arvyy.islisp.runtime;

import com.github.arvyy.islisp.Utils;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.staticobject.DefaultStaticObjectFactory;
import com.oracle.truffle.api.staticobject.StaticProperty;
import com.oracle.truffle.api.staticobject.StaticShape;

import java.util.List;

/**
 * Represents class created through defclass.
 *
 * @param name class name
 * @param parents parent classes
 * @param shape truffle shape to create instances from
 * @param slots defclass slots
 * @param isAbstract abstract flag
 * @param sourceSection associated source section
 */
@ExportLibrary(InteropLibrary.class)
public record StandardClass(
        String name,
        LispClass[] parents,
        StaticShape<DefaultStaticObjectFactory> shape,
        Slot[] slots,
        boolean isAbstract,
        SourceSection sourceSection
) implements LispClass, TruffleObject {

    /**
     * Standard class' slot information.
     *
     * @param name slot name
     * @param property associated truffle property for manipulation
     * @param initForm initialization form
     * @param initArg initialization argument
     */
    public record Slot(
            Symbol name,
            StaticProperty property,
            LispFunction initForm,
            Symbol initArg
    ) { }

    @Override
    public List<LispClass> getParents() {
        return List.of(parents);
    }

    @Override
    @ExportMessage
    public SourceSection getSourceLocation() {
        return sourceSection;
    }

    @ExportMessage
    boolean hasSourceLocation() {
        return sourceSection != null;
    }

    @ExportMessage
    boolean hasMembers() {
        return true;
    }

    @ExportMessage
    boolean isMemberReadable(String memberName) {
        switch (memberName) {
            case "superclasses":
            case "slots":
            case "abstract":
            case "name":
                return true;
            default:
                return false;
        }
    }

    @ExportMessage
    LispVector getMembers(boolean ignored) {
        return new LispVector(new Object[] {
           "superclasses",
           "slots",
           "abstract",
           "name"
        });
    }

    @ExportMessage
    Object readMember(String member) {
        switch (member) {
            case "superclasses" -> {
                var objArr = new Object[parents.length];
                System.arraycopy(parents, 0, objArr, 0, objArr.length);
                return new LispVector(objArr);
            }
            case "slots" -> {
                var objArr = new Object[slots.length];
                for (int i = 0; i < objArr.length; i++) {
                    objArr[i] = slots[i].name();
                }
                return new LispVector(objArr);
            }
            case "abstract" -> {
                return Utils.booleanToSymbol(isAbstract);
            }
            case "name" -> {
                return name;
            }
            default -> {
                return null;
            }
        }
    }

}
