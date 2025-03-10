# Makefile

TARGET = pydec
SRC = src/main.rs

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(SRC)
	rustc $< -o $@

c:
	export RUST_BACKTRACE=1 && cargo run contracts/Contract.sol

build: 
	cargo build --release

clean:
	cargo clean

t:
	cargo test

run: 
	./target/release/pydeC contracts/Contract.sol
