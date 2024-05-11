# Dockerfile used as part of github action workflow to publish already built linux binary
FROM debian:bookworm
COPY islisp-linux /app/islisp
RUN chmod +x /app/islisp
ENTRYPOINT ["/app/islisp"]