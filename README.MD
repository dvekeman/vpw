Target GHC Version is 7.8.3

Note for Mac OS X: 7.8.3 only works if you disable System Integrity Protection.
Change the resolver in stack.yaml and run

```
stack setup
stack build
```

# Building and running using GHC

```
ghc --make -Wall -O2 -o app.exe app.hs
./app.exe
```

# Building and running using stack

```
stack build
stack exec <blocks-exe, minmax1-exe, mixmax2-exe, ...>
# or 
stack ghci 
```

To build a single file with the GHC provided by stack 

```
cd <app1, blocks, minmax, ...>
stack ghc -- --make -Wall -O2 app.exe app.hs
```
