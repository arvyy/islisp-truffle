package com.github.arvyy.islisp.parser;

import com.oracle.truffle.api.source.SourceSection;

/**
 * Exception thrown when failing to parse a number during read.
 */
public class NumberParsingException extends ParsingException {

    private final String input;

    /**
     * Constructor.
     *
     * @param sourceSection location, if known.
     * @param input faulty string input that failed parsing.
     */
    public NumberParsingException(SourceSection sourceSection, String input) {
        super(sourceSection, "Failed to parse number from string: " + input);
        this.input = input;
    }

    /**
     * @return input for which parsing failed.
     */
    public String getInput() {
        return input;
    }

}
