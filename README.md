A (soon-to-be) extremely overengineered banner generator for TeamSpeak 3

Uses Haskell and Scotty, for now.

To compile,
```
$ git clone git@github.com:taktoa/TSBannerGen.git
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

To run, just run the resultant TSBannerGen binary

Right now the codebase is so unstable that I don't feel comfortable giving much more documentation.

[![Build Status](https://travis-ci.org/taktoa/TSBannerGen.svg?branch=master)](https://travis-ci.org/taktoa/TSBannerGen)
[![Build Status](https://drone.io/github.com/taktoa/TSBannerGen/status.png)](https://drone.io/github.com/taktoa/TSBannerGen/latest)
[![Coverage Status](https://coveralls.io/repos/taktoa/TSBannerGen/badge.png)](https://coveralls.io/r/taktoa/TSBannerGen)
