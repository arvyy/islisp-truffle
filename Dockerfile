# Dockerfile for building image locally
FROM container-registry.oracle.com/graalvm/native-image:25
RUN microdnf install -y maven
COPY . /app
WORKDIR /app
RUN mvn -Pnative -DskipTests=true package

FROM debian:trixie
COPY --from=0 /app/launcher/target/islisp /app/islisp
ENTRYPOINT ["/app/islisp"]
