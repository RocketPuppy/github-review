# github-review

Command line tool for reviewing Github pull requests.

## Setup

There are two ways of building this project.

1) Use Nix to create a sandbox with only your build tools in it. In this case
run `nix-build --arg nixpkgs "import <nixpkgs> {}"`, passing in your own package
set if necessary.

2) Do a normal cabal installation. In this case simply run `cabal install`.

## Hacking

I've provided a `shell.nix` file, so you can use `nix-shell --arg nixpkgs
"import <nixpkgs> {}"` to get an environment with everything loaded correctly.
