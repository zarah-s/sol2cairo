# Makefile

TARGET = pydec
SRC = src/main.rs

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(SRC)
	rustc $< -o $@

c:
	cargo run contracts/Contract.sol

build: 
	cargo build --release

clean:
	cargo clean

t:
	cargo test

run: 
	./target/release/pydeC contracts/Contract.sol
