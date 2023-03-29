
```sh
# Create dev env
opam switch create . 4.14.1 --deps-only

# Invoke vmtranslator
dune exec vmtranslator [in.vm] [out.asm]
```
