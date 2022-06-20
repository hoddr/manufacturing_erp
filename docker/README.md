# build process

hoddr, 2021
hoddr@outlook.com

- in order to build for debian 10 server (buster), haskell has to be compiled
on similar os (C library dynamic/static links, etc)
- while other options exist, simply running a docker for build seems simplest at the moment

1. setup docker
2. run `./configure`
  - first time will take a LONG time
  - this is setting up debian instance with ghc, cabal, postgresql, etc as needed for build
  - this "base" image will build your project at its current code stage
  - do this so that subsequent builds (i.e. releases or hotfixes) take a fraction of the time
  - base image should rarely (if ever) need updates, except for ghc/cabal version updates or new
    server OS
3. run `./build` for new release
  - JUST builds the project at current repo state
  - theoretically could use this image to actually run the server (with env vars, etc)
  - would need to expose inner docker port to outer port (4000)
4. run `./extract` for pulling built prod release files from docker image
  - output to `prod_dist/dist-newstyle`

- these built files can be sftp'd to the relevant server(s) along with web file changes
- process managers restarted
- test new release deployment
