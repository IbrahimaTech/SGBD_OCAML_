<<<<<<< HEAD
# OCaml Mini-RDBMS — Functional Dependencies & Normalization

A relational database engine written and implemented in pure OCaml.

The project implements basic relational algebra (`insert`, `prod`, `projection`, `restrict`) and a normalization analyzer (`compute_deps`, `compute_elementary_deps`, `normalization_level`) that automatically detects whether a table is in 1NF, 2NF or 3NF.

## Project layout

```
.
├── Makefile         # Build, run and clean targets
├── README.md
├── .gitignore
├── projet.ml        # Types, helpers, relational algebra, normalization
└── tests.ml         # Test tables and expected-behavior assertions
```

## Constraints respected by the implementation

In line with the course requirements, every list traversal is written using **only** `List.map`, `List.fold_left` and `List.fold_right`:

- no `List.filter`, no `List.mem`, no `List.iter`, no `List.exists`, …
- no `option` types
- no `when` guards in pattern matching
- explicit accumulators everywhere recursion is needed.

The basic list functions that were unavailable (`for_all`, `rev`, `filter`, `appartient`) are reimplemented at the top of `projet.ml` exclusively with `List.fold_left` / `List.map`.

## Build

```bash
make            # builds the test executable (bytecode)
make run        # builds then runs the tests
make toplevel   # opens an interactive OCaml toplevel with projet.ml loaded
make clean      # removes every build artifact
```

The build collapses `projet.ml` and `tests.ml` into a single OCaml source file (`_build/all.ml`) — the `#use "projet.ml"` directive at the top of `tests.ml` is only understood by the toplevel, so we strip it for the compiler.

## Running the tests interactively

If you prefer the toplevel:

```bash
ocaml
# then in the toplevel:
#use "tests.ml";;
```

This loads `projet.ml` (via the `#use` directive at the top of `tests.ml`) and immediately runs all the test blocks.

## What the tests cover

- **`check_table`** on an invalid table containing a forbidden `VNull`
- **`check_table`** on an invalid table with an incompatible value type
- **`insert`** on a valid row, an incompatible-type row, a `VNull` in a non-nullable column, and a row with the wrong arity
- **`restrict`** on the `commande` table with the predicate `n_comm = "C01"`
- **`projection`** of `commande` on `n_produit` and `type`
- **`prod`** computing the cartesian product `commande × produit`
- **`compute_elementary_deps`** and **`normalization_level`** on three reference tables that are respectively in 3NF (`produit`), 2NF (`commande`) and 1NF (`inscription`).

## Author

Ibrahima Diaby — ENSIIE, first year Computer Science & Computer Systems Engineering Students — INPF12 / PRIM11 (2025-2026).

=======
# SGBD_OCAML_
>>>>>>> fbdaf88cd10d440e61e86c80d313f32d4fcc1407
