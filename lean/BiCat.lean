/- BiCat.lean -/

import Setoid
import Cat
import Functor

namespace EXE
namespace Bicat

    -- carrier of a bicategory: type of morphisms
    abbreviation HomType (Ob : Type) : Type := Π(Dom Cod : Ob), CatType
    -- print HomType

    -- structure of a bicategory (operations of degree 0)
    section withHom
        variables {Ob : Type} (Hom : HomType Ob)
        abbreviation IdType : Type :=
            Π{a : Ob}, Hom a a
        abbreviation MulType : Type :=
            Π{a b c : Ob}, Hom b c ⟶ Hom a b ⟶ Hom a c
    end withHom

    -- extra structure of bicategory (operations of degree 1)
    section withIdMul
        variables {Ob : Type} {Hom : HomType Ob}
        variables (Id : IdType Hom) (Mul : MulType Hom)

        abbreviation UnitLMor : Type :=
            Π{a b : Ob}, Π(f : Hom a b), (Mul $$ Id $$ f) ⇒(Hom a b)⇒ f
        abbreviation UnitRMor : Type :=
            Π{a b : Ob}, Π(f : Hom a b), (Mul $$ f $$ Id) ⇒(Hom a b)⇒ f
        abbreviation UnitCMor : Type :=
            Π{a : Ob}, (Mul $$ Id $$ Id) ⇒(Hom a a)⇒ (@Id a)
        abbreviation UnitLInvMor : Type :=
            Π{a b : Ob}, Π(f : Hom a b), f ⇒(Hom a b)⇒ (Mul $$ Id $$ f)
        abbreviation UnitRInvMor : Type :=
            Π{a b : Ob}, Π(f : Hom a b), f ⇒(Hom a b)⇒ (Mul $$ f $$ Id)
        abbreviation UnitCInvMor : Type :=
            Π{a : Ob}, (@Id a) ⇒(Hom a a)⇒ (Mul $$ Id $$ Id)
        abbreviation AssocMor : Type :=
            Π{a b c d : Ob}, Π(f : Hom c d), Π(g : Hom b c), Π(h : Hom a b),
                (Mul $$ (Mul $$ f $$ g) $$ h) ⇒(Hom a d)⇒ (Mul $$ f $$ (Mul $$ g $$ h))
        abbreviation AssocInvMor : Type :=
            ∀{a b c d : Ob}, Π(f : Hom c d), Π(g : Hom b c), Π(h : Hom a b),
                (Mul $$ f $$ (Mul $$ g $$ h)) ⇒(Hom a d)⇒ (Mul $$ (Mul $$ f $$ g) $$ h)

    end withIdMul


end Bicat
end EXE
