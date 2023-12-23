package com.github.arvyy.islisp.buildinfo;

import java.io.IOException;
import java.util.Properties;

/**
 * Allows introspecting meta information about the build.
 */
public class BuildInfo {

    public static Properties getBuildProperties() {
        var properties = new Properties();
        try {
            try (var stream = BuildInfo.class.getResourceAsStream(
                "git-islisp-lang.properties")
            ) {
                properties.load(stream);
            }
        } catch (IOException e) {
            throw new RuntimeException("Failed to load build properties", e);
        }
        return properties;
    }

}
