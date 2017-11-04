/- DepSet.lean -/

import Setoid
import Cat
import Functor
import Over

set_option pp.universes true
set_option pp.metavar_args false

namespace EXE

-- main

definition DepSetoidCat (Base : SetoidType) : CatType := (Cat.FromSet Base) ⟶ SetoidCat

definition OverSetoidCat (Base : SetoidType) : CatType := OverCat SetoidCat Base

-- utils

definition DepId {Base: SetoidType} (P : DepSetoidCat Base)
    {b1 b2 : Base} (eq12 : b1 ≡_≡ b2) (eq21 : b2 ≡_≡ b1)
        : Setoid.Id ≡_≡ ((P $$/ eq21) ∙ (P $$/ eq12)) :=
    SetoidType.Trans3 ((P $$ b1)⥤(P $$ b1))
        (SetoidType.Sym ((P $$ b1)⥤(P $$ b1)) (FunctorType.onId P))
        (P $$// (@id (⊜ ≡(Setoid.FromType (b1 ≡_≡ b1))≡ (eq12 ⊡_⊡ eq21)) true.intro))
        (FunctorType.onMul P eq21 eq12)

definition DepAdj {Base: SetoidType} (P : DepSetoidCat Base)
    {b1 b2 : Base} (eq12 : b1 ≡_≡ b2) (eq21 : b2 ≡_≡ b1)
    (f1 : [P $$ b1]) (f2 : [P $$ b2])
        : (((P $$/ eq12) $ f1) ≡_≡ f2) → (f1 ≡_≡ ((P $$/ eq21) $ f2)) :=
    λ eqP, SetoidType.Trans (P $$ b1)
        ((DepId P eq12 eq21) /$ f1)
        ((P $$/ eq21) $/ eqP)

end EXE
