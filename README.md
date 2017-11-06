# module-counter

While looking to determine what percentage of a codebase was actually being tested, I found it suprising that I couldn't enumerate the exported modules (at least I couldn't easily figure out how). It wasn't too difficult for a `cabal` project, but `stack` didn't make it easy. So, this tiny app enumerates all exported modules in a stack project, potentially across many cabal projects.

### Running it

```
> git clone git@github.com:collegevine/module-counter
> cd module-counter && stack install
> module-counter-exe
```
