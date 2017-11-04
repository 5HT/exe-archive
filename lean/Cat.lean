/- Cat -/

import Setoid

set_option pp.universes true
set_option pp.metavar_args false

namespace EXE
namespace Cat

    -- carrier of a category: type of morphisms
    abbreviation HomType (Ob : Type) : Type := Π(Dom Cod : Ob), SetoidType

    -- structure of a category
    section withHom
        variables {Ob : Type} (Hom : HomType Ob)
        abbreviation IdType : Type :=
            Π{a : Ob}, Hom a a
        abbreviation MulType : Type :=
            Π{a b c : Ob}, Hom b c ⥤ Hom a b ⥤ Hom a c
    end withHom

    -- axioms of category
    section withIdMul
        variables {Ob : Type} {Hom : HomType Ob}
        variables (Id : IdType Hom) (Mul : MulType Hom)
        abbreviation UnitLProp : Prop :=
            ∀{a b : Ob}, ∀(f : Hom a b), (Mul Id $ f) ≡(Hom a b)≡ f
        abbreviation UnitRProp : Prop :=
            ∀{a b : Ob}, ∀(f : Hom a b), (Mul f $ Id) ≡(Hom a b)≡ f
        abbreviation UnitCProp : Prop :=
            ∀{a : Ob}, (Mul Id $ Id) ≡(Hom a a)≡ Id
        abbreviation UnitLInvProp : Prop :=
            ∀{a b : Ob}, ∀(f : Hom a b), f ≡(Hom a b)≡ (Mul Id $ f)
        abbreviation UnitRInvProp : Prop :=
            ∀{a b : Ob}, ∀(f : Hom a b), f ≡(Hom a b)≡ (Mul f $ Id)
        abbreviation UnitCInvProp : Prop :=
            ∀{a : Ob}, Id ≡(Hom a a)≡ (Mul Id $ Id)
        abbreviation AssocProp : Prop :=
            ∀{a b c d : Ob}, ∀(f : Hom c d), ∀(g : Hom b c), ∀(h : Hom a b),
                (Mul (Mul f $ g) $ h) ≡(Hom a d)≡ (Mul f $ (Mul g $ h))
        abbreviation AssocInvProp : Prop :=
            ∀{a b c d : Ob}, ∀(f : Hom c d), ∀(g : Hom b c), ∀(h : Hom a b),
                (Mul f $ (Mul g $ h)) ≡(Hom a d)≡ (Mul (Mul f $ g) $ h)
    end withIdMul

end Cat

namespace Setoid

    -- identity in the category `Setoid`
    definition Id : Cat.IdType HomSet :=
        λ (A : SetoidType), MkHom
            /- onEl -/ ( λ (a : A), a)
            /- onEqu -/ ( λ (a1 a2 : A), λ (e12 : a1 ≡_≡ a2), e12)

    definition Mul : Cat.MulType HomSet :=
        λ(A B C : SetoidType), MkHom2
            (B⥤C) (A⥤B) (A⥤C)
            /- onElEl -/ ( Setoid.Mul.onElEl )
            /- onElEqu -/ ( λ(f : B⥤C), λ(g1 g2 : A⥤B), λ(g12 : g1 ≡(A⥤B)≡ g2),
                    λ(a : A), f (g12 a))
            /- onEquEl -/ ( λ(f1 f2 : B⥤C), λ(f12 : f1 ≡(B⥤C)≡ f2), λ(g : A⥤B),
                    λ(a : A), f12 (g a))

    definition UnitL : Cat.UnitLProp @Id @Mul :=
        λ(A B : SetoidType), λ(f : A⥤B), λ(a : A), ⊜
    definition UnitR : Cat.UnitRProp @Id @Mul :=
        λ(A B : SetoidType), λ(f : A⥤B), λ(a : A), ⊜
    definition Assoc : Cat.AssocProp @Mul :=
        λ(A B C D : SetoidType), λ(f : C⥤D), λ(g : B⥤C), λ(h : A⥤B), λ(a : A), ⊜

end Setoid

-- objects of 2-category `Cat`
record CatType : Type :=
    (Ob : Type)
    (Hom : Cat.HomType Ob)
    (Id : Cat.IdType Hom)
    (Mul : Cat.MulType Hom)
    (UnitL : Cat.UnitLProp @Id @Mul)
    (UnitR : Cat.UnitRProp @Id @Mul)
    (Assoc : Cat.AssocProp @Mul)
print CatType
abbreviation Cat.MkOb := CatType.mk

-- carrier of category
attribute CatType.Ob [coercion]
notation ` ⟦ ` C ` ⟧ ` := CatType.Ob C
notation a ` ⇒` C `⇒ ` b := CatType.Hom C a b
notation ` ① ` := CatType.Id _
notation f ` ⊙` C `⊙ ` g := CatType.Mul C f $ g

abbreviation CatType.UnitC (C : CatType)
    : Cat.UnitCProp (@CatType.Id C) (@CatType.Mul C) :=
    λ (X : C), CatType.UnitL C (@CatType.Id C X)

abbreviation CatType.UnitLInv (C : CatType)
    : Cat.UnitLInvProp (@CatType.Id C) (@CatType.Mul C) :=
    λ X Y, λ f, SetoidType.Sym _ (CatType.UnitL C f)

abbreviation CatType.UnitRInv (C : CatType)
    : Cat.UnitRInvProp (@CatType.Id C) (@CatType.Mul C) :=
    λ X Y, λ f, SetoidType.Sym _ (CatType.UnitR C f)

abbreviation CatType.UnitCInv (C : CatType)
    : Cat.UnitCInvProp (@CatType.Id C) (@CatType.Mul C) :=
    λ (X : C), SetoidType.Sym _ (@CatType.UnitC C X)

abbreviation CatType.AssocInv (C : CatType)
    : Cat.AssocInvProp (@CatType.Mul C) :=
    λ X Y Z T, λ f g h, SetoidType.Sym _ (CatType.Assoc C f g h)

definition CatType.MulHE (C : CatType) {X Y Z : C}
    (mYZ : Y ⇒C⇒ Z)
    {mXY1 mXY2 : X ⇒C⇒ Y}(e12 : mXY1 ≡(X ⇒C⇒ Y)≡ mXY2)
        : (mYZ ⊙C⊙ mXY1) ≡(X ⇒C⇒ Z)≡ (mYZ ⊙C⊙ mXY2) :=
    (CatType.Mul C $ mYZ) $/ e12

definition CatType.MulEH (C : CatType) {X Y Z : C}
    {mYZ1 mYZ2 : Y ⇒C⇒ Z}(e12 : mYZ1 ≡(Y ⇒C⇒ Z)≡ mYZ2)
    (mXY : X ⇒C⇒ Y)
        : (mYZ1 ⊙C⊙ mXY) ≡(X ⇒C⇒ Z)≡ (mYZ2 ⊙C⊙ mXY) :=
    (CatType.Mul C $/ e12) /$ mXY

notation f ` ⊙` C `⊙/ ` geq := CatType.MulHE C f geq
notation feq ` /⊙` C `⊙ ` g := CatType.MulEH C feq g


-- the category of `Setoid`s
definition SetoidCat : CatType :=
    Cat.MkOb
        SetoidType Setoid.HomSet
        @Setoid.Id @Setoid.Mul
        @Setoid.UnitL @Setoid.UnitR @Setoid.Assoc

namespace Cat
    definition FromSet (S : SetoidType) : CatType :=
        let Hom (x y : S) : SetoidType := Setoid.FromType (x ≡S≡ y) in
        Cat.MkOb
            /- Ob -/ S
            /- Hom -/ Hom
            /- Id -/ ( λ (x : S), SetoidType.Refl S)
            /-Mul -/ ( λ (x y z : S), Setoid.MkHom2
                (Hom y z) (Hom x y) (Hom x z)
                ( λ (yz : y ≡S≡ z), λ (xy : x ≡S≡ y), SetoidType.Trans S xy yz)
                ( λ yz xy1 xy2 xyE, true.intro)
                ( λ yz1 yz2 yzE xy, true.intro))
            /- UnitL -/ ( λ x y, λ f, true.intro)
            /- UnitR -/ ( λ x y, λ f, true.intro)
            /- Assoc -/ ( λ x y z t, λ f g h, true.intro)
end Cat

namespace CatType
    record IsoHom (C : CatType) (A B : C) : Type :=
        (AtoB : A ⇒C⇒ B)
        (BtoA : B ⇒C⇒ A)
        (atA : (BtoA ⊙C⊙ AtoB) ≡(A ⇒C⇒ A)≡ ①)
        (atB : (AtoB ⊙C⊙ BtoA) ≡(B ⇒C⇒ B)≡ ①)
    abbreviation MkIso {C : CatType} {A B : C} := @IsoHom.mk C A B
end CatType

notation a ` ⇐` C `⇒ ` b := CatType.IsoHom C a b

infix ` ⇔ `:10 := CatType.IsoHom SetoidCat

end EXE
