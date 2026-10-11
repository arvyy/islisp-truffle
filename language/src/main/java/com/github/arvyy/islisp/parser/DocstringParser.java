package com.github.arvyy.islisp.parser;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
* Class for parsing single docstring to constituent parts.
*/
public final class DocstringParser {

    /**
    * Structured result of the docstring.
    * @param description free form textual description
    * @param returnType return type informat if any
 */
    public static record Result(
        String description,
        String returnType
    ) { }

    private DocstringParser() { }

    private static final Pattern RETURN_BLOCK = Pattern.compile("^\\s*@return (.*)$");

    /**
     * Parse docstring.
     * @param input docstring
     * @return parsed result
     */
    public static Result parseDocstring(String input) {
        var desc = new StringBuilder();
        String returnType = null;
        for (var line: input.split("\\n")) {
            Matcher m;
            m = RETURN_BLOCK.matcher(line);
            if (m.matches()) {
                returnType = m.group(1);
                continue;
            }
            desc.append(line).append("\n");
        }
        return new Result(desc.toString(), returnType);
    }

}
