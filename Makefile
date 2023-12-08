.PHONY: clean, dist

dist:
	mkdir -p dist
	mvn -DskipTests=true clean package
	cp launcher/target/*.jar dist

dist/islisp-static: dist
	${JAVA_HOME}/bin/native-image \
	    --static --libc=musl\
	    --macro:truffle-svm \
	    --no-fallback \
	    --initialize-at-build-time \
	    -p dist/language-1.0-SNAPSHOT.jar:dist/launcher-1.0-SNAPSHOT.jar:dist/polyglot-23.1.1.jar:dist/truffle-api-23.1.1.jar:dist/nativeimage-23.1.1.jar:dist/truffle-runtime-23.1.1.jar:dist/truffle-compiler-23.1.1.jar:dist/jniutils-23.1.1.jar:dist/commons-cli-1.6.0.jar \
	    com.github.arvyy.islisp.launcher.Main \
	    dist/islisp-static

clean:
	rm -rf dist
	mvn clean
