# Repo Setup

Bare setup for getting the server up and running. Local installs of `ghc`, `cabal`, `postgresql`, and `node` are required. There are a few local (global) installs for command-line
utilities as well. This - and all docs - assume the reader has a decent amount of understanding in regard to web apps, database management, and general dev ops concepts.

## Haskell

Recommend using [ghcup](https://www.haskell.org/ghcup/).

- [ghc](https://downloads.haskell.org/ghc/latest/docs/html/users_guide)
- [cabal](https://cabal.readthedocs.io/en/3.6/getting-started.html)

At the moment, version `ghc-8.10.7` and `cabal-3.6.2.0` are used (Windows at `ghc-8.10.4`). Versions must
be honored or the build will likely have issues (due to library requirments).

## Node

Recommend using [nvm](https://github.com/nvm-sh/nvm).

- [node](https://nodejs.org/en/docs/)

## Cache-Busting (via Haskell)

See [cache_buster_gen.hs](../cache_buster_gen.hs).

To ensure web pages force the user's browser to get updated data, a simple cache-busting mechanism is appended to urls via querystrings. These cache keys are generated via a
`uuid-v4` as part of the web build process. The required Haskell library must be installed and accessible via local command-line. See the source code for a note regarding the
required install(s) through `cabal`.

## Postgresql

Current major version: `13` or `14`. Follow your distribution's recommendations to install and setup `postgres`. The Arch Linux wiki has good documentation for the
[setup](https://wiki.archlinux.org/title/PostgreSQL).

Create a database with the desired name (this will match an environment variable later) and a user (probably best to match your username!) that has full privileges to said
database.

A database backup file provides a quick start, but the migrations can also be run in numerical order (low-to-high) to get the skeleton in place. The former is recommended for most,
if not all, cases.

## Environment Variables

The following environment variables are required. A local settings example is provided.

```
ERP_PB_BASE_URL="http://localhost:4000"
ERP_PB_DBUSER="foobar"
ERP_PB_DBPASS=""
ERP_PB_DBPORT=5432
ERP_PB_DBNAME="erp"
ERP_PB_DBHOST="localhost"
```

*Note: the port for the server should be moved to an environment variable as well.*

## Initial Build

There are a variety of helper scripts provided in the [scripts](../scripts/.) folder. For the initial build, it is recommended to run `cabal update` first, then you may run:

```shell
cabal update
cabal build
./web-build
./run
```

If you are not in a `bash` shell, you can look at the scripts themselves to determine what commands to run to mimic their behavior.

*Note: if this is the first time you've built, it will take a long time. Subsequent builds should be rather quick.*
