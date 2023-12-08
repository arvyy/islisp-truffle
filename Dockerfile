FROM vegardit/graalvm-maven:21.0.1
RUN apt-get update
RUN apt-get -y install make
ADD http://more.musl.cc/10/x86_64-linux-musl/x86_64-linux-musl-native.tgz /musl.tgz
RUN mkdir /musl && tar -xf /musl.tgz -C /musl
ENV TOOLCHAIN_DIR=/musl/x86_64-linux-musl-native
ADD https://zlib.net/zlib-1.3.tar.gz /zlib.tar.gz
RUN mkdir /zlib && tar -xf /zlib.tar.gz -C /zlib
WORKDIR /zlib/zlib-1.3
ENV CC=$TOOLCHAIN_DIR/bin/gcc
RUN ./configure --prefix=$TOOLCHAIN_DIR --static &&\
    make &&\
    make install
COPY . /app
WORKDIR /app
ENV PATH=$PATH:$TOOLCHAIN_DIR/bin
RUN make dist/islisp-static

FROM alpine:3
COPY --from=0 /app/dist/islisp-static /app/islisp
ENTRYPOINT ["/app/islisp"]