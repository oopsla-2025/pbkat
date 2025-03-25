# Probabilistic analysis of quantum networks

## Development environment

  * install [Nix][nix] package manager
  * enable [Nix flakes][flakes]
  * **to enter environment run** `nix develop` from the artifact's root
  * run `hpack` (no arguments) to generate `.cabal` file

## Building

```bash
cabal build
```

## Running the tool

Examples are stored inside `probabilistic-examples/` in files named `%protoname%.hs` that define:

 * protocol expression in variable `p` 
 * probabilities of actions in variable `actionConfig`
 * network capacity constraints in variable `networkCapacity`
 * event, whose probability of success we're interested in variable `ev`

### Computing convex sets

To print generated convex sets corresponding to the protocol of `%protoname%.hs` run:

```bash
cabal run prob%protoname% -- run
```

To dump [JSON][json] representation of generated convex sets corresponding to the protocol of `%protoname%.hs` run:

```bash
cabal run prob%protoname% -- run --json > %protoname%.json
```

### Computing probabilities

After having generated [JSON][json] representation of convex sets corresponding to the protocol of `%protoname%.hs`, one can run:

```bash
# use --json to produce machine-readable output
caval run prob%protoname% -- probability [--json] <%protoname%.json
```

[nix]: https://nixos.org/download
[flakes]: https://nixos.wiki/wiki/Flakes#Other_Distros.2C_without_Home-Manager
[json]: https://www.json.org/json-en.html
