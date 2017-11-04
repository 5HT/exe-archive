Require    Import Strings.String.

Inductive  Loc: Type := intro: string -> nat -> nat -> Loc.

Definition Ident     := string.
Definition Label     := string.
Definition Binder    := prod Ident Loc.
Definition Branch    := prod Label (prod (list Binder) Type).
Definition NamedRec  := list (prod Binder Type).
Definition NamedSum  := list (prod Binder NamedRec).
Definition DecList   := list (prod Binder (prod Type Type)).

Inductive Decls      := Intro:  DecList -> Decls
                      | Opaq:   Binder  -> Decls
                      | Transp: Binder  -> Decls.
