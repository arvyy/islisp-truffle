.PHONY: clean

define manifest
Bundle-Name: ISLISP
Bundle-Symbolic-Name: com.github.arvyy.islisp
Bundle-Version: 23.0.1
Bundle-RequireCapability: org.graalvm; filter:="(&(graalvm_version=23.0.1)(os_arch=amd64))"
x-GraalVM-Working-Directories: languages/islisp
endef
export manifest


dist/islisp.jar:
	mkdir -p dist
	mvn -DskipTests=true -P shade clean package
	cp language/target/islisp.jar dist

dist/islisp-launcher.jar:
	mkdir -p dist
	mvn -DskipTests=true -P shade clean package
	cp launcher/target/islisp.jar dist/islisp-launcher.jar

dist/islisp-component.jar: dist/islisp.jar
	mkdir -p dist/component/languages/islisp
	cp dist/islisp.jar dist/component/languages/islisp
	mkdir -p dist/component/META-INF
	@echo "$$manifest" > dist/component/META-INF/MANIFEST.MF
	${JAVA_HOME}/bin/jar -c -f dist/islisp-component.jar -m dist/component/META-INF/MANIFEST.MF -C dist/component/ .

dist/islisp: dist/islisp-launcher.jar
	${JAVA_HOME}/bin/native-image --macro:truffle --no-fallback --initialize-at-build-time -cp dist/islisp-launcher.jar com.github.arvyy.islisp.launcher.Main dist/islisp

clean:
	rm -rf dist
	mvn clean
