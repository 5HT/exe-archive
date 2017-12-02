Infinity Language
=================

Layered Core
------------

The idea of Infinity Language came from the need to unify and
arrange different calculi as extensions to the core of the
language with dependent types (or MLTT core). While working on
distributed systems, data storages and stream processing, we discovered that two core
languages: pi calculus and stream calculus were
connected and being driven by a language with quantifiers as primitives.

### PTS

The language has pure functions, infinity number of universes,
fixpoint, homotopy interval [0,1], inductive types and pi/sigma as a core.

### MLTT

With usage of dependent fibrations and Pi, Sigma types Martin-Lof introduced MLTT.
Which is known to be sound and consistent with only one impredicative contractable
bottom space and predicative hierarchy of infinitely many spaces. It can be used both as
an operational calculus and a logical framework. It is the core of top level AST while
other languaages are represented as leafs of this core.

### HTS

Homotopy calculus is an extension of MLTT with path types, intervals and circles and HITs.
It is used to model higher inductive types and stands as a contemporary math foundation.

You can read about Infinity Language layers at http://groupoid.space/mltt/infinity/

Base Library
------------

This library is dedicated to <b>cubical</b>-compatible
typecheckers based on homotopy interval
<b>[0,1]</b> and MLTT as a core. The base library is founded
on top of 5 core modules: <b>proto</b> (composition, id, const),
<b>path</b> (subst, trans, cong, refl, singl, sym),
<b>propset</b> (isContr, isProp, isSet),
<b>equiv</b> (fiber, eqiuv) and <b>iso</b> (lemIso, isoPath).
This machinery is enough to prove univalence axiom.

(i) The library has rich recursion scheme primitives
in lambek module, while very basic nat, list, stream
functionality. (ii) The very basic theorems are given
in pi, iso_pi, sigma, iso_sigma, retract modules.
(iii) The library has category theory theorems from
HoTT book in cat, fun and category modules.
(iv) The library also includes some impredicative
categorical encoding sketches in coproduct_set.
lambek also includes inductive semantics modeled
with cata/ana recursion and fixpoint adjoints in/out.

This library is best to read with HoTT book at http://groupoid.space/mltt/types/

![depgr](https://github.com/groupoid/infinity/blob/master/doc/img/base.png?raw=true)
