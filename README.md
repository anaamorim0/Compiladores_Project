# Compiladores_Project

# Kotlin Compiler — From Scanner to MIPS Code Generation

This project is divided into two major stages, which together form a basic compiler for a subset of the **Kotlin programming language**.


## Overview

The compiler implements:
- A **scanner** (lexical analyzer)
- A **parser** (syntax analyzer)
- **AST** construction (Abstract Syntax Tree)
- A **symbol table** with semantic information
- An **intermediate code generator** (three-address code)
- A **MIPS code generator**, compatible with [MARS simulator]


## Supported Kotlin Subset

The language subset considered includes:
### Expressions
- Arithmetic and boolean expressions
- `print()` and `readln()` function calls

### Statements
- Variable declarations and assignments
- `if / else` conditionals
- `while` loops
- A `main` function as the entry point



## Part 1 — Scanner, Parser, and AST

- Lexical analysis: identifies valid tokens  
- Syntax analysis: validates structure and builds the AST  
- Output: an abstract representation of the program, in-memory or textual


## Part 2 — Semantic Analysis, TAC & MIPS Code Generation

- Symbol table to manage variables and their types  
- Generation of **three-address code (TAC)** from the AST  
- MIPS code output (`.asm`), suitable for simulation in **MARS**
