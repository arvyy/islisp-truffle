# Dockerfile for building image locally
FROM container-registry.oracle.com/graalvm/native-image:22
RUN microdnf install -y maven
COPY . /app
WORKDIR /app
RUN mvn -Pnative -DskipTests=true package

FROM debian:bookworm
COPY --from=0 /app/launcher/target/islisp-linux /app/islisp
ENTRYPOINT ["/app/islisp"]