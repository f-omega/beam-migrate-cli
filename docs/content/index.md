# `beam-migrate-cli`: CLI tool for `beam-migrate`

[`beam-migrate`](https://hackage.haskell.org/package/beam-migrate) provides a generic backend for
the implementation of frontend migrations tools. `beam-migrate-cli` is designed to work with
`beam-migrate` to enable principled migrations for `beam` databases.

## Installation

`beam-migrate-cli` can be installed via `stack` or `cabal`.

```bash
cabal install beam-migrate-cli
# or
stack install beam-migrate-cli
```

## Documentation

All documentation here assumes familiarity with the [Beam Documentation](https://haskell-beam.github.io/beam).

If you're new to `beam-migrate-cli` you should read the [Tutorial](tutorial/part1).

Advanced users may be interested in the [User Guide](user-guide).

## Features

Non-exhaustive list of features:

* **Multi-user workflows** - Modern software projects are the result of teams of
  collaborators. Database changes are often made simultaneously by multiple authors. `beam-migrate`
  detects when this happens and tries to determine whether the changes conflict. Final approval for
  a reworked migration is up to the user.
* **Embedding into client apps** - Not all databases live on servers. Often times apps rely on their
  own internal database, or self-hosted web services must manage their own hosted db. The
  `beam-migrate pickle` command can generate a Haskell module that you can compile into your
  application for seamless migrations in distributed software.
* **Raw SQL** - DDL languages differ widely by databases and it wouldn't be feasible to express
  every possibility as a Haskell DSL. `beam-migrate-cli` recognizes this fact of the industry and
  uses raw SQL text to store migrations (can be backend-specific).
* **Schema synchronization** - `beam-migrate-cli` can be used to verify that the `beam` schema used
  to access a database is compatible with the schema specified in the migrations. Additionally, each
  migration can contain its own verification script, for application-specific requirements
* **`git` integration** - `beam-migrate` integrates well with `git` (other VCs to come). This allows
  identification of individual migrations with `git` branches. You can have multiple versions of
  your database in different branches. `beam-migrate-cli` offers a principled way to handle merges
  and database changes, all with the certainty of `beam-migrate`'s verification mechanisms to ensure
  your code runs successfully at run-time.
* **Automated code generation for Haskell** - Point `beam-migrate-cli` at your database, and get a
  `beam` compatible schema that you can use to access it.
* **Automatic migration generation** - Already have a database schema you want to copy? Or have you
  changed your database, but don't remember the changes? No problem `beam-migrate-cli`'s `diff`
  command will automatically figure out the changes made between two databases or migrations and
  generate a migration script for you.
* **Multiple backends** - `beam-migrate-cli` is built directly atop `beam-migrate`, which means
  it'll support any backend that `beam-migrate` supports, including third-party ones.
* **CI integration** - `beam-migrate-cli` is perfect for CI tools. Automatically keep your database
  in sync while ensuring that your database matches what your running tools expect.

Planned features:

* **Support for non-Haskell languages** - `beam-migrate-cli` will eventually be able to generate
  schemas to access a beam database for various non-Haskell frameworks. Perfect for data type
  sharing between Haskell backends and your frontend.
* **Non-`git` VCs** - `beam-migrate-cli` will support non-`git` VCs, such as bazaar, mercurial, and
  darcs. Contributions welcome.


## Contributions

Contributions are welcome! Please open PRs.

## Bug reporting

Please report bugs using the GitHub bug tracker

## Licensing

`beam-migrate-cli` is licensed under the MIT License, copied below:

```
Copyright 2025 F Omega Enterprises LLC and Travis Athougies

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```
