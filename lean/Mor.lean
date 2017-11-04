/- Mor.lean -/

import Setoid
import Cat

namespace EXE

/-
 - The category of morphisms
 -/

record MorType (C : CatType) : Type :=
    (Dom : C)
    (Cod : C)
    (Hom : Dom ⇒C⇒ Cod)

namespace Mor

    -- commutative squares
    abbreviation SquareProp
        (C : CatType) {X11 X12 X21 X22 : C}
        (m1x : X11 ⇒C⇒ X12) (m2x : X21 ⇒C⇒ X22)
        (mx1 : X11 ⇒C⇒ X21) (mx2 : X12 ⇒C⇒ X22)
            : Prop :=
        (mx2 ⊙C⊙ m1x) ≡(X11 ⇒C⇒ X22)≡ (m2x ⊙C⊙ mx1)

    definition SquareId1
         (C : CatType) {X1 X2 : C} (m : X1 ⇒C⇒ X2)
            : SquareProp C m m ① ① :=
        (CatType.UnitL C m) ⊡_⊡ (CatType.UnitRInv C m)

    definition SquareMul1
        (C : CatType) {X11 X12 X21 X22 X31 X32 : C}
        {m1x : X11 ⇒C⇒ X12} {m2x : X21 ⇒C⇒ X22} {m3x : X31 ⇒C⇒ X32}
        {ma1 : X11 ⇒C⇒ X21} {mb1 : X21 ⇒C⇒ X31}
        {ma2 : X12 ⇒C⇒ X22} {mb2 : X22 ⇒C⇒ X32}
        (sq12 : SquareProp C m1x m2x ma1 ma2)
        (sq23 : SquareProp C m2x m3x mb1 mb2)
            : SquareProp C m1x m3x (mb1 ⊙C⊙ ma1) (mb2 ⊙C⊙ ma2) :=
        (CatType.Assoc C mb2 ma2 m1x) ⊡_⊡
        (mb2 ⊙C⊙/ sq12) ⊡_⊡
        (CatType.AssocInv C mb2 m2x ma1) ⊡_⊡
        (sq23 /⊙C⊙ ma1) ⊡_⊡
        (CatType.Assoc C m3x mb1 ma1)

    definition SquareId2
        (C : CatType) {X1 X2 : C} (m : X1 ⇒C⇒ X2)
            : SquareProp C ① ① m m :=
        (CatType.UnitR C m) ⊡_⊡ (CatType.UnitLInv C m)

    -- morphisms in the category of morphisms
    record HomType (C : CatType) (A B : MorType C) : Type :=
        (atDom : MorType.Dom A ⇒C⇒ MorType.Dom B)
        (atCod : MorType.Cod A ⇒C⇒ MorType.Cod B)
        (sq : SquareProp C (MorType.Hom A) (MorType.Hom B) atDom atCod)

    definition HomEqu (C : CatType) {A B : MorType C} : EquType (HomType C A B) :=
        λ(f g : HomType C A B),
            ( (HomType.atDom f ≡(MorType.Dom A ⇒C⇒ MorType.Dom B)≡ HomType.atDom g) ∧
              (HomType.atCod f ≡(MorType.Cod A ⇒C⇒ MorType.Cod B)≡ HomType.atCod g) )

    definition HomSet (C : CatType) : Cat.HomType (MorType C) :=
        λ(A B : MorType C), Setoid.MkOb
            /- El -/ (HomType C A B)
            /- Equ -/ (@HomEqu C A B)
            /- Refl -/ (λ(f : HomType C A B), (and.intro ⊜ ⊜))
            /- Trans -/ (λ(f g h : HomType C A B),
                λ(fg : HomEqu C f g), λ(gh : HomEqu C g h), (and.intro
                    (and.elim_left fg ⊡_⊡ and.elim_left gh)
                    (and.elim_right fg ⊡_⊡ and.elim_right gh)))
            /- Sym -/ (λ(f g : HomType C A B),
                λ(fg : HomEqu C f g), (and.intro
                    (SetoidType.Sym _ (and.elim_left fg))
                    (SetoidType.Sym _ (and.elim_right fg))))

    definition Id (C : CatType) : Cat.IdType (@HomSet C) :=
        λ(A : MorType C),
            HomType.mk ① ① (SquareId1 C (MorType.Hom A))

    definition Mul (C : CatType) : Cat.MulType (@HomSet C) :=
        λ(m1 m2 m3 : MorType C), Setoid.MkHom2
            (HomSet C m2 m3) (HomSet C m1 m2) (HomSet C m1 m3)
            /- onElEl -/ (λ(mm23 : HomSet C m2 m3), λ(mm12 : HomSet C m1 m2), HomType.mk
                /- atDom -/ (HomType.atDom mm23 ⊙C⊙ HomType.atDom mm12)
                /- atCod -/ (HomType.atCod mm23 ⊙C⊙ HomType.atCod mm12)
                /- sq -/ (SquareMul1 C (HomType.sq mm12) (HomType.sq mm23)))
            /- onElEqu -/ (λ(mm23 : HomSet C m2 m3), λ(mm12 mm12' : HomSet C m1 m2),
                λ(eq : mm12 ≡(HomSet C m1 m2)≡ mm12'), and.intro
                    ((HomType.atDom mm23) ⊙C⊙/ (and.elim_left eq))
                    ((HomType.atCod mm23) ⊙C⊙/ (and.elim_right eq)))
            /- onEquEl -/ (λ(mm23 mm23' : HomSet C m2 m3),
                λ(eq : mm23 ≡(HomSet C m2 m3)≡ mm23'),
                λ(mm12 : HomSet C m1 m2), and.intro
                    ((and.elim_left eq) /⊙C⊙ (HomType.atDom mm12))
                    ((and.elim_right eq) /⊙C⊙ (HomType.atCod mm12)))

    definition UnitL (C : CatType) : Cat.UnitLProp (@Id C) (@Mul C) :=
        λ(m1 m2 : MorType C), λ(m12 : HomType C m1 m2), and.intro
            (@CatType.UnitL C (MorType.Dom m1) (MorType.Dom m2) (HomType.atDom m12))
            (@CatType.UnitL C (MorType.Cod m1) (MorType.Cod m2) (HomType.atCod m12))

    definition UnitR (C : CatType) : Cat.UnitRProp (@Id C) (@Mul C) :=
        λ(m1 m2 : MorType C), λ(m12 : HomType C m1 m2), and.intro
            (@CatType.UnitR C (MorType.Dom m1) (MorType.Dom m2) (HomType.atDom m12))
            (@CatType.UnitR C (MorType.Cod m1) (MorType.Cod m2) (HomType.atCod m12))

    definition Assoc (C : CatType) : Cat.AssocProp (@Mul C) :=
        λ(m1 m2 m3 m4: MorType C),
        λ(m34 : HomType C m3 m4),
        λ(m23 : HomType C m2 m3),
        λ(m12 : HomType C m1 m2),
        and.intro
            (@CatType.Assoc C
                (MorType.Dom m1) (MorType.Dom m2)
                (MorType.Dom m3) (MorType.Dom m4)
                (HomType.atDom m34) (HomType.atDom m23) (HomType.atDom m12))
            (@CatType.Assoc C
                (MorType.Cod m1) (MorType.Cod m2)
                (MorType.Cod m3) (MorType.Cod m4)
                (HomType.atCod m34) (HomType.atCod m23) (HomType.atCod m12))

end Mor

definition MorCat (C : CatType) : CatType :=
    Cat.MkOb
        (MorType C) (Mor.HomSet C)
        (@Mor.Id C) (@Mor.Mul C)
        (@Mor.UnitL C) (@Mor.UnitR C) (@Mor.Assoc C)

end EXE
