# Part 2

In the last section you built a tool and a checked database schema. Now, let's use
`beam-migrate-cli` to generate add a new migration for the initial schema.

## The `add` command

Migrations are added to a project by using the `add` command. Since this will be your first
migration, `beam-migrate-books` will create a `beam-migrate-cli` directory structure in the current
folder. This consists of a `.beam-migrate` file (called the 'registry') and a `beam_migrations`
directory, which contains the migrations.

```bash
cabal v2-run beam-migrate-books -- add 
```
