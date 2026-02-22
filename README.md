# cabal-stackage

A Stack-like wrapper for cabal-install that uses
[Stackage](https://www.stackage.org) snapshots to provide reproducible,
well-constrained Haskell builds — without the rebuild churn and stale snapshot
problems that come with Stack's default behaviour.

Instead of maintaining its own solver, cabal-stackage fetches the `cabal.config`
constraint file that Stackage publishes for each snapshot and imports it into a
generated `cabal.project.stackage` file, then delegates all actual building to
`cabal-install`.  This means you get the full power of cabal's solver and build
system with Stackage's curated, tested package set.

## How it works

1. Fetches `https://www.stackage.org/download/snapshots.json` (cached for
   60 minutes) to resolve snapshot aliases such as `lts` or `lts-24` to their
   current pinned version (e.g. `lts-24.31`).
2. Downloads the corresponding
   `https://www.stackage.org/lts-24.31/cabal.config` (cached indefinitely for
   exact versions) to `~/.cache/cabal-stackage/`.
3. Locates the GHC version specified in the config — first on `PATH`, then in
   Stack's `~/.stack/programs/` directory — and passes it to cabal via `-w`.
4. Generates `cabal.project.stackage` in the project root, importing the cached
   config (and your existing `cabal.project` if present).
5. Runs `cabal --project-file=cabal.project.stackage <command> <args>`.

## Usage

```
cabal-stackage COMMAND [-s|--snapshot SPEC] [--debug] [ARGS]
```

### Commands

| Command | Description |
|---|---|
| `build` | Build the project |
| `install` | Install the project |
| `test` | Run the test suite |
| `run` | Run an executable |
| `repl` | Open a REPL |
| `haddock` | Build documentation |
| `clean` | Clean build artifacts |
| `snapshot [SPEC]` | Show the current snapshot, or set it |
| `refresh` | Force re-download of the snapshot config |
| `build-all [SPECS...]` | Build against multiple snapshots (like stack-all) |

`build`, `install`, `test`, `run`, `repl`, `haddock`, and `clean` all accept:

- `-s/--snapshot SPEC` — override the configured snapshot for this invocation
- `--debug` — print the resolved snapshot, compiler path, and exact cabal
  command before running it

Extra arguments are forwarded to cabal. Use `--` to pass cabal flags:

```
cabal-stackage build -- -j4
```

### Snapshot spec format

Both `lts-24` and `lts24` (no dash) are accepted for major version specs.

| Spec | Meaning |
|---|---|
| `lts` | Latest LTS snapshot |
| `lts-24` or `lts24` | Latest minor of LTS 24 |
| `lts-24.31` | Exact pinned snapshot |
| `nightly` | Latest nightly |
| `nightly-2026-02-21` | Specific nightly |

## Configuration

### Pinning a snapshot

Pin a snapshot for your project by running:

```
cabal-stackage snapshot lts-24
```

This writes (or updates) a `.cabal-stackage` file in the project root. Commit
this file to version control — it is the equivalent of Stack's `resolver:`
field in `stack.yaml`.

A global fallback can be set in `~/.config/cabal-stackage/resolver`.

Priority order: `--snapshot` flag > `.cabal-stackage` > global config > `lts`.

### .cabal-stackage file

The `.cabal-stackage` file supports three optional fields:

```
resolver: lts-24
newest:   lts-24
oldest:   lts-21
```

`newest` and `oldest` set the LTS major version bounds used by `build-all`
when no explicit snapshot list is given (see below). They are the equivalent of
the `newest`/`oldest` fields in stack-all's `.stack-all` file.

### Package version overrides

To use a different version of a package than what Stackage pins, add one or
more `constraints:` lines to `.cabal-stackage`:

```
resolver: lts-24
constraints: aeson ==2.1.0.0
constraints: text >=2.1
```

cabal-stackage removes the Stackage-pinned constraint for each overridden
package and adds yours in its place before running cabal. This is necessary
because cabal treats constraints conjunctively — you cannot simply add a
second constraint for the same package on top of Stackage's.

Constraints can also be placed in per-snapshot config files (see below) to
apply only to specific snapshots.

### Per-snapshot config files

For `build-all` (and for single-snapshot builds with an explicit `--snapshot`),
cabal-stackage also looks for a per-snapshot config file alongside `.cabal-stackage`.
The filename is `.cabal-stackage.<suffix>` where the suffix matches the snapshot:

| File | Applies to |
|---|---|
| `.cabal-stackage.nightly` | `nightly` |
| `.cabal-stackage.lts24` | `lts-24` (any minor) |
| `.cabal-stackage.lts24.31` | `lts-24.31` (exact) |

These files use the same key-value format as `.cabal-stackage`. The `resolver:`
field is optional — it can be omitted since the snapshot is already implied by
the filename. Fields in the per-snapshot file take precedence over the base
`.cabal-stackage` file.

This is the equivalent of stack-all's `stack-nightly.yaml` / `stack-lts23.yaml`
per-snapshot overrides. Commit these files alongside `.cabal-stackage`.

A common use case is pinning a package to a different version for a specific
snapshot only:

```
# .cabal-stackage.nightly  -- use a pre-release version of a package on nightly
constraints: some-package ==1.3.0.0
```

```
# .cabal-stackage.lts21  -- an older snapshot needs a compat shim
constraints: some-package ==1.0.0.0
```

Constraints in a per-snapshot file replace (not accumulate with) any
constraint for the same package in the base `.cabal-stackage`.

## build-all

`build-all` is the equivalent of
[stack-all](https://hackage.haskell.org/package/stack-all): it builds your
project against a range of Stackage snapshots in sequence, starting from
nightly and working down through LTS majors.

```
# Build from nightly down to lts-16 (default range)
cabal-stackage build-all

# Override bounds on the command line
cabal-stackage build-all --newest lts-24 --oldest lts-21

# Build against an explicit list of snapshots
cabal-stackage build-all nightly lts-24 lts-23
```

When no explicit list is given, the range is controlled by (in priority order):

1. `--newest`/`--oldest` CLI flags
2. `newest`/`oldest` fields in `.cabal-stackage`
3. Default: nightly down to lts-16

Nightly is always included at the top of the default range regardless of
`newest`/`oldest` bounds.

## GHC discovery

cabal-stackage reads the `with-compiler:` field from the Stackage config and
locates the matching GHC automatically:

1. Searches `PATH` for `ghc-X.Y.Z`
2. Falls back to Stack's `~/.stack/programs/<arch>/ghc-*-X.Y.Z/bin/ghc`,
   covering Stack's variant naming (e.g. `ghc-tinfo6-9.10.3`)

If neither is found a warning is printed and cabal is left to resolve the
compiler itself.

## Generated files

`cabal.project.stackage` is written to your project root on every run and
should be added to `.gitignore`.  It looks like:

```
-- Generated by cabal-stackage, do not edit
packages: .
import: /home/user/.cache/cabal-stackage/lts-24.31.config
```

If a `cabal.project` already exists it is imported instead of the bare
`packages: .` line.

## Installation

```
cabal install cabal-stackage
```

Requires `cabal-install` to be on your `PATH`.

## Comparison with Stack

| | Stack | cabal-stackage |
|---|---|---|
| Solver | Custom (Stack's own) | cabal-install |
| Snapshot pinning | `stack.yaml` `resolver:` | `.cabal-stackage` `resolver:` |
| Latest minor auto-update | No (explicit bump needed) | Yes (via snapshots.json) |
| Uses system GHC | Optional | Yes |
| Stack-installed GHC fallback | Yes | Yes |
| cabal.project support | Limited | Full |
| build-all / multi-snapshot | stack-all (separate tool) | Built-in |
| Per-snapshot config overrides | `stack-lts23.yaml` etc. | `.cabal-stackage.lts23` etc. |
| Package version overrides | `extra-deps` in stack.yaml | `constraints:` in `.cabal-stackage` |
