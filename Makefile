# Makefile

TARGET = compiler
SRC = src/main.rs

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(SRC)
	rustc $< -o $@

compile:
	cargo run contracts/Contract.sol

build: 
	cargo build --release

clean:
	cargo clean

t:
	cargo test

run: 
	./target/release/solc contracts/Contract.sol
