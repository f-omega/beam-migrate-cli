# Part 1

This tutorial will help you set up the `beam-migrate-cli` tool in your own project.

## Prerequisites

This tutorial assumes you are familiar with the [Beam Documentation](https://haskell-beam.github.io/beam).

While not a strict requirement, you also need to have an existing `beam`-compatible schema.

For the sake of the tutorial, we will deal with the following schema:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
module Database.Beam.Migrate.Tutorial.Schema where

import Database.Beam
import Database.Beam.Migrate

import Data.Text (Text)

data BookT f
    = Book
    { _bookId :: C f Text
    , _bookName :: C f Text
    , _bookAuthor :: PrimaryKey AuthorT f
    , _bookYearPublished :: C f Int
    , _bookPageCount :: C f Int
    } deriving (Generic, Beamable)

type Book = BookT Identity
deriving instance Show Book

instance Table BookT where
  data PrimaryKey BookT f = BookKey (C f Text)
    deriving (Generic, Beamable)
  primaryKey = BookKey <$> _bookId

type BookKey = PrimaryKey BookT Identity
deriving instance Show BookKey

data AuthorT f
    = Author
    { _authorId :: C f (SqlSerial Int)
    , _authorName :: C f Text
    } deriving (Generic, Beamable)

type Author = AuthorT Identity
deriving instance Show Author

instance Table AuthorT where
  data PrimaryKey AuthorT f = AuthorKey (C f (SqlSerial Int))
    deriving (Generic, Beamable)
  primaryKey = AuthorKey <$> _authorId

type AuthorKey = PrimaryKey AuthorT Identity
deriving instance Show AuthorKey

data GenreT f
    = Genre
    { _genreId :: C f (SqlSerial Int)
    , _genreName :: C f Text
    } deriving (Generic, Beamable)

type Genre = GenreT Identity
deriving instance Show Genre

instance Table GenreT where
  data PrimaryKey GenreT f = GenreKey (C f (SqlSerial Int))
    deriving (Generic, Beamable)
  primaryKey = GenreKey <$> _genreId

type GenreKey = PrimaryKey GenreT Identity
deriving instance Show GenreKey

data BookGenreT f
    = BookGenre
    { _bgBook :: PrimaryKey BookT f
    , _bgGenre :: PrimaryKey GenreT f
    } deriving (Generic, Beamable)

instance Table BookGenreT where
  newtype PrimaryKey BookGenreT f = BookGenreKey (BookGenreT f)
    deriving (Generic, Beamable)
  primaryKey = BookGenreKey

data BookDb f
    = BookDb
    { _dbBooks :: f (TableEntity BookT)
    , _dbAuthors :: f (TableEntity AuthorT)
    , _dbGenres :: f (TableEntity GenreT)
    , _dbBookGenres :: f (TableEntity BookGenreT)
    } deriving (Generic)

instance Database be BookDb
```

## Getting started

In order to make a beam database compatible with `beam-migrate` we need to create a
`CheckedDatabaseSettings` object. This is the same data type in principle as `DatabaseSettings`
which we construct (and perhaps modify) using `defaultDbSettings` in beam. `CheckedDatabaseSettings`
contains additional metadata about the database schema (constraints, explicit data types, etc).

The easiest way to construct a `CheckedDatabaseSettings` is using
`defaultMigratableDbSettings`. This sets up a `CheckedDatabaseSettings` with sensible defaults,
using the same naming scheme as `defaultDbSettings`. Similarly, it can be modified in the same way.

```haskell
bookDbChecked :: CheckedDatabaseSettings be BookDb
bookDbChecked = defaultMigratableDbSettings
```

Of course, you'll need a `DatabaseSettings` in order to use the database. Simply use the `unChecked`
function from `Database.Beam.Migrate`.

```haskell
bookDb :: DatabaseSettings be BookDb
bookDb = unChecked bookDbChecked
```

## Set up

`beam-migrate-cli` is distributed as a Haskell library package. You link with the `beam-migrate-cli`
library to create a `beam-migrate-cli` executable that is specific to your database. The library
offers easy hooks to customize your CLI tool to whatever workflow you use in your project.

First, create a new executable in your Haskell project. If you're using cabal, you can do something
like this in your `.cabal` file:

```
executable beam-migrate-books
  main-is: Main.hs
```

Then in `Main.hs`

```haskell
module Main where

import Database.Beam.Migrate.Cli

import qualified Database.Beam.Sqlite.Migrate as Sqlite
import qualified Database.Beam.Postgres.Migrate as Pg

import Database.Beam.Migrate.Tutorial.Schema (bookDbChecked)

main = beamMigrateCli (defBeamMigrateOptions
  & registerBackend Sqlite.migrationBackend bookDbChecked
  & registerBackend Pg.migrationBackend bookDbChecked)
```

That's it! The `beamMigrateCli` function takes care of all the CLI argument parsing and database
handling. You simple supply a `BeamMigrateOptions` set up the way you like. Here, we take the
default options `defBeamMigrateOptions` and register two new database backends, for `sqlite` and
`postgres`. You can set up as many backends as you like depending on your projects need. Each
backend is associated with a partcular schema. Since schemas may change from backend to backend,
each database backend independently registers its schema. This offers a way to customize the schema
in a backend-specific way, although we do not do that here.

## Testing it out

You can now compile and run your tool:

```bash
cabal v2-run beam-migrate-books -- --help
```

You should see output like the following:

```
Usage: beam-migrate-cli-test [--show-commands] [(-q|--quiet) | (-v|--verbose)]
                             [(-C|--connstr CONNSTR) | (-D|--database NAME)]
                             (COMMAND | COMMAND | COMMAND)

  beam-migrate - Manage beam migrations

Available options:
  --show-commands          Show commands as they're run
  -q,--quiet               Only output necessary information
  -v,--verbose             Output copious information
  -C,--connstr CONNSTR     Database connection string to use
  -D,--database NAME       Name of database to connect to
  -h,--help                Show this help text

Available commands:
  status                   Get status of migration registry
  log                      Show changes made to database
  db                       Manage the database directly

Workflow Commands:
  add                      Add a new migration to the registry
  abort                    Abort a migration in progress
  migrate                  Get status of migration registry
  commit                   Commit a migration to the registry
  edit                     Edit files for migration
  pickle                   Generate a Haskell module containing a snapshot of
                           the registry and migrations database

Schema Management:
  verify                   Get status of migration registry
  diff                     Get difference between two database specs
```

Continue to [Part 2](part2) to set up your first migration.
