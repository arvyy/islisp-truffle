# Dockerfile used as part of github action workflow to publish already built linux binary
FROM debian:bookworm
COPY dist/islisp-linux /app/islisp
ENTRYPOINT ["/app/islisp"]