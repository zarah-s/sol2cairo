# Solidity → Cairo Compiler(sol2cairo)

🚧 **Under Active Development** 🚧

This project is a compiler that translates **Solidity smart contracts** into **Cairo** code, making it possible for Ethereum developers to seamlessly deploy and run their contracts on **Starknet**.

---

## ✨ Mission

We aim to make **Ethereum developers instantly compatible with Starknet** by compiling Solidity into Cairo, bridging existing contracts and tooling with the Starknet ecosystem.

---

## 📌 Why This Matters

- Ethereum developers have a huge base of contracts and tools written in **Solidity**.
- Starknet, built on Cairo, requires smart contracts to be written in a completely different language.
- Rewriting contracts from scratch is inefficient and slows adoption.

✅ Our compiler removes this barrier by **translating Solidity directly to Cairo (sierra, casm, IR)**, ensuring developers can bring their apps to Starknet with minimal friction.

---

## 🛠️ Compiler Pipeline

The compilation process follows a classic multi-stage pipeline:

1. **Lexical Analysis (Lexer)** – Breaks Solidity code into tokens.
2. **Parsing** – Consumes tokens and applies grammar rules.
3. **AST (Abstract Syntax Tree)** – Structured tree representation of the contract.
4. **Semantic Analysis** – Type checking, scope resolution, and validation of correctness.
5. **Intermediate Representation (IR)** – Cairo program IR generation.
6. **Casm Generation** – Produces Casm bytecode suitable for Starknet execution.

---

---

## ✅ Current Progress

- [x] Lexical analysis
- [x] Parser + AST generation
- [ ] Semantic analysis (in progress)
- [ ] Cairo IR generation
- [ ] Casm backend
- [ ] CLI + developer tooling

---

## 🔮 Roadmap

- Implement semantic checks (types, variables, scopes)
- Build Cairo IR generator
- Extend backend to output Casm
- Add CLI for easy compilation
- Provide developer tooling and test suite

---

## 📖 Planned Usage

Once stable, you will be able to run:

```bash
sol2cairo MyContract.sol

```
