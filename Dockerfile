FROM ghcr.io/graalvm/native-image-community:22.0.0
RUN microdnf install -y ant maven
COPY . /app
WORKDIR /app
RUN ant -v dist native
RUN chmod +x dist/islisp-linux

FROM debian:bookworm
COPY --from=0 /app/dist/islisp-linux /app/islisp
ENTRYPOINT ["/app/islisp"]