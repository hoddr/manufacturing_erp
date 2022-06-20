# Production Builds

There are two supported methods for deployment - `Docker` and remote instance. The latter is used exclusively at the moment, as a Windows workstation and `Docker` don't play well
together.

The production server is a `Debian 10` instance. Due to linked, static C files, the Haskell build must be completed on an equivelent distribution (to the best of current
knowledge). There are a few [helper scripts](../scripts/.) to make this build, `scp` down, `scp` up, deploy process about as automated as possible. Start with the [full build
script](../scripts/full-build) for details. `ssh` support for all servers is needed. For security, do NOT use passwords over ssh, just public/private key pairs.

## nginx

A `nginx` proxy server is setup in front of the Haskell server. The configuration can be found [here](../remote/.). Some tweaks will be required for a new server instance. For
`SSL` certification, the server leverages [certbot](https://certbot.eff.org/).

## Instance

The remote server is currently setup through [Digital Ocean](https://www.digitalocean.com/). Should access to the server ever fail, the instance can be retrieved through the
account (DC has the credentials). You can also tweak the memory and RAM of the instance there.

## General Setup

The remote instance also requires setup of `Postgresql` and environment variables. The environment variables are set in `/etc/environment` and will require
restart of the server instance for changes to take effect.

## Docker Build

This process was the first build process, permitting a Debian build on a non-Debian Linux distribution. It has not been tested in some time, but should be useable with some work.
Details can be found [here](../docker/README.md).
