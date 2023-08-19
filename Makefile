.PHONY: clean

dist/islisp.jar:
	mkdir -p dist
	mvn -DskipTests=true -P shade clean package
	cp launcher/target/islisp.jar dist

dist/islisp: dist/islisp.jar
	${JAVA_HOME}/bin/native-image --macro:truffle --no-fallback --initialize-at-build-time -cp dist/islisp.jar com.github.arvyy.islisp.launcher.Main dist/islisp

clean:
	rm -rf dist
	mvn clean
