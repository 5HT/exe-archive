Infinity Language
=================

Base Library
------------

![depgr](https://github.com/groupoid/infinity/blob/master/doc/img/baselib.png?raw=true)

The idea of Infinity Language came from the need to unify and
arrange different calculi as extensions to the core of the
language with dependent types (or MLTT core). While working on
distributed systems, data storages and stream processing, we discovered that two core
languages: pi calculus and stream calculus were
connected and being driven by a language with quantifiers as primitives.

Infinity is a dependently typed language for stream processing,
distributed processing and text processing. We use Cubical to prototype
the standard library including the theories needed to
prove code invariants instead of testing them. Infinity is a source
to source translator which lets you use different languages at runtime:
Erlang, Rust, Clang, Futhark, Julia, etc.

Infinity language presents a solid and unified way of modeling inter-language
computations and inter-system communications within a single language with
compact core. We strive for utilizing parallel hardware such as GPU and
SSE/AVX SIMD instructions and providing a robust and verified distributed
environment with process and channels runtime.

Simple Lambda Calculus
----------------------

Simple lambda calculus was discovered by Church as computational model of functions.
This is the simplest model of lambda calculus in lambda cube, also it suffers from paradoxes.
To help escape this the simple typed lambda caclulus was invented.

The Simple Lambda Calculus is used to model dynamic interpreted
target languages such as JavaScript, Erlang, Lua, K and other shells.
A total fragment of call by value lambda calculus is used in the optimizer
as a runtime model. Translation to dynamic languages requires type erasure in
addition to syntactic transformations.

Hindley-Milner Type System
--------------------------

A Hindley-Milner type system is used to model static target languages such as C++ or Rust.
The system models certain features of low-level languages such as unboxed
types and non-first-class instantiation-based polymorphism pretty well.
The completeness and modularity of type inference lets the code generator
reconstruct the native types of the target language.

While Hindley-Milner systems in general and the particular set of extensions
used are pretty limited compared to System F, the chosen type system still
allows to model native effects and translate structural recursion and
corecursion into native iteration.

MLTT
----

With usage of dependent fibrations and Pi, Sigma types Martin-Lof introduced MLTT.
Which is known to be sound and consistent with only one impredicative contractable
bottom space and predicative hierarchy of infinitely many spaces. It can be used both as
an operational calculus and a logical framework. It is the core of top level AST while
other languaages are represented as leafs of this core.

Homotopy
--------

Homotopy calculus is an extension of MLTT with path types, intervals and circles.
It is used to model higher inductive types and stands as a contemporary math foundation.

Pi Calculus
-----------

Pi calculus can be obtained by a transformation from lambda
calculus by replacing a variable constructor with stream constructor.
But the language and protocol accessing this stream could vary
from backend to backend with respect to design requirements.

Pi calculus is also known as a process calculus that can be used to model of distributed system
over a channel transport. Each process is represented as a coinductive program and can 
run both in parallel and sequentially. Processes communicate by inductive protocols
known as session types over physical channels that can be corecursive streams (linear types),
random access arrays (static lifetime) or other effectful disciplines. Process calculus
is used to model distributed systems such as Erlang or application based protocols.

The process calculus itself should be backed with number of infinity streams,
represented as processor cores. The parallel system of reactive schedulers
that cycle the list of AST trees filled with calculus instructions.

Stream Calculus
---------------

Thus, stream calculus should provide with different disciplines for accessing
underlying streams used in Pi calculus. For example: 1) linear types, or streams with
constant direction without rollbacks where each element of stream is touched once;
2) random access arrays; 3) GPU sources; 4) runtime typed channels;
5) effect and coeffect streams (processes as streams). Such stream
calculuses could be landed with such GPU languages as Futhark
or AVX intrinsics languages, such as Julia. We could treat
stream calculus as memory representation with different protocol accesses.

One type of disciplines is the stream calculus. While pi calculus could be imagined as
lambda calculus where function arguments are channels or streams, the stream calculus defines a set
of constructions over these streams. This calculus is needed to provide different forms
of vectorization that can be used on GPU and AVX hardware.

Effect Calculus
---------------

The other type of discipline is effect calculus of streams and programs. It covers internal
process states, `try` `raise` `catch` constructions, `random` and other infinity streams.
Also this subtree contains I/O and low level operations that are linked with runtime.
