package com.github.arvyy.islisp.runtime;

import com.github.arvyy.islisp.ISLISPTruffleLanguage;
import com.github.arvyy.islisp.parser.LocalScopeVariable;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

import java.util.List;

/**
 * Gives variable information during debugger breakpoint.
 */
@ExportLibrary(InteropLibrary.class)
public class DebuggerScope implements TruffleObject {

    private final Frame frame;
    private final List<List<LocalScopeVariable>> scopeVariables;


    /**
     * Create debugger scope for given frame and scope definition.
     * @param frame
     * @param scopeVariables
     */
    public DebuggerScope(Frame frame, List<List<LocalScopeVariable>> scopeVariables) {
        this.frame = frame;
        this.scopeVariables = scopeVariables;
    }

    @ExportMessage
    boolean hasMembers() {
        return true;
    }

    @ExportMessage
    boolean isScope() {
        return true;
    }

    @ExportMessage
    boolean hasScopeParent() {
        return scopeVariables.size() > 1;
    }

    @ExportMessage
    Object getScopeParent() {
        return new DebuggerScope(frame, scopeVariables.subList(1, scopeVariables.size()));
    }

    @ExportMessage
    Object getMembers(boolean ignored) {
        return getMembersBoundary();
    }

    @ExportMessage
    @CompilerDirectives.TruffleBoundary
    Object readMember(String s) {
        for (List<LocalScopeVariable> scopeVariable : scopeVariables) {
            var maybeValue = scopeVariable
                .stream()
                .filter(e -> s.equals(e.name()))
                .findAny()
                .map(v -> getObject(v.frameIndex(), v.slot()));
            if (maybeValue.isPresent()) {
                return maybeValue.get();
            }
        }
        return null;
    }

    Object getObject(int frameIndex, int frameSlot) {
        Frame f = frame;
        for (int i = 0; i < frameIndex; i++) {
            f = ((Closure) f.getArguments()[0]).frame();
        }
        return f.getObject(frameSlot);
    }

    @ExportMessage
    boolean isMemberReadable(String ignored) {
        return true;
    }

    @CompilerDirectives.TruffleBoundary
    Object getMembersBoundary() {
        var arr = scopeVariables
            .stream()
            .flatMap(lst -> lst.stream())
            .map(e -> e.name())
            .toArray(String[]::new);
        return new ScopeMembers(arr);
    }

    @ExportMessage
    Object toDisplayString(boolean ignored) {
        return "test";
    }

    @ExportMessage
    boolean hasLanguage() {
        return true;
    }

    @ExportMessage
    Class<ISLISPTruffleLanguage> getLanguage() {
        return ISLISPTruffleLanguage.class;
    }

}

@ExportLibrary(InteropLibrary.class)
class ScopeMembers implements TruffleObject {

    final String[] members;

    ScopeMembers(String[] members) {
        this.members = members;
    }

    @ExportMessage
    boolean hasArrayElements() {
        return true;
    }

    @ExportMessage
    long getArraySize() {
        return members.length;
    }

    @ExportMessage
    boolean isArrayElementReadable(long index) {
        return index < members.length;
    }

    @ExportMessage
    Object readArrayElement(long index) {
        return members[(int) index];
    }

}
