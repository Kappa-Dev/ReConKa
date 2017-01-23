Replay a kappa trace and extract connectivity information

---

In order to compile, you need to `opam pin --dev add KaSim` [1] and
you can then `ocamlbuild ReConKa.native` (or `ocamlbuild ReConKa.byte`
if necessary).

---

`./ReConKa -help` should tell you what you need to know.

[1] KaSim requires Ocaml >= 4.02.3. Opam will take care of installing
other dependencies.