package com.github.arvyy.islisp.parser;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DocstringParser {

    public static record Result(
        String description,
        String returnType
    ) {}

    private DocstringParser() { }

    private final static Pattern RETURN_BLOCK = Pattern.compile("^\\s*@return (.*)$");
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
