FROM rust:alpine AS builder
WORKDIR /app
COPY . .
RUN cargo build --release

FROM alpine
COPY --from=builder /app/target/release/compilers-project /compiler
COPY src /src
EXPOSE 3000
CMD ["/compiler", "serve", "--host=0.0.0.0"]
