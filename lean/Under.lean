/- Under.lean -/

import Setoid
import Cat
import Functor

namespace EXE

/-
 - The category over X
 -/

record UnderType (C : CatType) (X : C) : Type :=
    (Cod : C)
    (Hom : X ⇒C⇒ Cod)

namespace Under

    -- commutative triangles
    abbreviation TriProp
        (C : CatType) (X : C) {X1 X2 : C}
        (m1 : X ⇒C⇒ X1)
        (m2 : X ⇒C⇒ X2)
        (m12: X1 ⇒C⇒ X2)
            : Prop :=
        m2 ≡(X ⇒C⇒ X2)≡ (m12 ⊙C⊙ m1)

    definition TriId
        (C : CatType) (X : C) {X1 : C} (m : X ⇒C⇒ X1)
            : TriProp C X m m ① :=
        CatType.UnitLInv C m

    definition TriMul
        (C : CatType) (X : C) {X1 X2 X3 : C}
        {m1 : X ⇒C⇒ X1} {m2 : X ⇒C⇒ X2} {m3 : X ⇒C⇒ X3}
        {m12 : X1 ⇒C⇒ X2} {m23 : X2 ⇒C⇒ X3}
        (tr12 : TriProp C X m1 m2 m12)
        (tr23 : TriProp C X m2 m3 m23)
            : TriProp C X m1 m3 (m23 ⊙C⊙ m12) :=
        tr23 ⊡_⊡
        (m23 ⊙C⊙/ tr12) ⊡_⊡
        (CatType.AssocInv C m23 m12 m1)

    -- morphisms in the category of morphisms
    record HomType (C : CatType) (X : C) (A B : UnderType C X) : Type :=
        (atCod : UnderType.Cod A ⇒C⇒ UnderType.Cod B)
        (tr : TriProp C X (UnderType.Hom A) (UnderType.Hom B) atCod)

    definition HomEqu (C : CatType) (X : C) {A B : UnderType C X} : EquType (HomType C X A B) :=
        λ(f g : HomType C X A B),
            (HomType.atCod f ≡(UnderType.Cod A ⇒C⇒ UnderType.Cod B)≡ HomType.atCod g)

    definition HomSet (C : CatType) (X : C) : Cat.HomType (UnderType C X) :=
        λ(A B : UnderType C X), Setoid.MkOb
            /- El -/ (HomType C X A B)
            /- Equ -/ (@HomEqu C X A B)
            /- Refl -/ ( λ(f : HomType C X A B), ⊜)
            /- Trans -/ ( λ(f g h : HomType C X A B),
                λ(fg : HomEqu C X f g), λ(gh : HomEqu C X g h),
                    (fg ⊡_⊡ gh))
            /- Sym -/ ( λ(f g : HomType C X A B),
                λ(fg : HomEqu C X f g),
                    (SetoidType.Sym _ fg))

    definition Id (C : CatType) (X : C) : Cat.IdType (@HomSet C X) :=
        λ(A : UnderType C X),
            HomType.mk ① (TriId C X (UnderType.Hom A))

    definition Mul (C : CatType) (X : C) : Cat.MulType (@HomSet C X) :=
        λ(m1 m2 m3 : UnderType C X), Setoid.MkHom2
            (HomSet C X m2 m3) (HomSet C X m1 m2) (HomSet C X m1 m3)
            /- onElEl -/ ( λ(mm23 : HomSet C X m2 m3), λ(mm12 : HomSet C X m1 m2), HomType.mk
                /- atCod -/ (HomType.atCod mm23 ⊙C⊙ HomType.atCod mm12)
                /- tr -/ (TriMul C X (HomType.tr mm12) (HomType.tr mm23)))
            /- onElEqu -/ ( λ(mm23 : HomSet C X m2 m3), λ(mm12 mm12' : HomSet C X m1 m2),
                λ(eq : mm12 ≡(HomSet C X m1 m2)≡ mm12'),
                    ((HomType.atCod mm23) ⊙C⊙/ eq))
            /- onEquEl -/ ( λ(mm23 mm23' : HomSet C X m2 m3),
                λ(eq : mm23 ≡(HomSet C X m2 m3)≡ mm23'),
                λ(mm12 : HomSet C X m1 m2),
                    (eq /⊙C⊙ (HomType.atCod mm12)))

    definition UnitL (C : CatType) (X : C) : Cat.UnitLProp (@Id C X) (@Mul C X) :=
        λ(m1 m2 : UnderType C X), λ(m12 : HomType C X m1 m2),
            (@CatType.UnitL C (UnderType.Cod m1) (UnderType.Cod m2) (HomType.atCod m12))

    definition UnitR (C : CatType) (X : C) : Cat.UnitRProp (@Id C X) (@Mul C X) :=
        λ(m1 m2 : UnderType C X), λ(m12 : HomType C X m1 m2),
            (@CatType.UnitR C (UnderType.Cod m1) (UnderType.Cod m2) (HomType.atCod m12))

    definition Assoc (C : CatType) (X : C) : Cat.AssocProp (@Mul C X) :=
        λ(m1 m2 m3 m4: UnderType C X),
        λ(m34 : HomType C X m3 m4),
        λ(m23 : HomType C X m2 m3),
        λ(m12 : HomType C X m1 m2),
            (@CatType.Assoc C
                (UnderType.Cod m1) (UnderType.Cod m2)
                (UnderType.Cod m3) (UnderType.Cod m4)
                (HomType.atCod m34) (HomType.atCod m23) (HomType.atCod m12))

end Under

definition UnderCat (C : CatType) (X : C) : CatType :=
    Cat.MkOb
        (UnderType C X) (Under.HomSet C X)
        (@Under.Id C X) (@Under.Mul C X)
        (@Under.UnitL C X) (@Under.UnitR C X) (@Under.Assoc C X)

definition Under.Forget (C : CatType) (X : C) : UnderCat C X ⟶ C :=
    Functor.MkOb
        (UnderType.Cod)
        ( λ(U1 U2 : UnderCat C X), Setoid.MkHom
            λ(m : U1 ⇒_⇒ U2), Under.HomType.atCod m
            )
        (sorry)
        (sorry)

print Under.Forget

end EXE
