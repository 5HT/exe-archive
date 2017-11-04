/- SetoidLim -/

import Setoid
import Mor
import Functor
import Adjunction
import Initial

set_option pp.universes true
set_option pp.metavar_args false

namespace EXE

/-
 - Definition of LIMIT in SetoidCat
 -/

namespace Setoid
    record LimType {C : CatType} (F : C⟶SetoidCat) : Type :=
        (atOb : Π(X : C), [F X])
        (atHom : ∀{X Y : C}, ∀(m : X ⇒C⇒ Y), (atOb Y) ≡(F Y)≡ ((F m) $ (atOb X)))
    abbreviation MkLim {C : CatType} {F : C⟶SetoidCat} := @LimType.mk C F
    print LimType
end Setoid

-- action
attribute Setoid.LimType.atOb [coercion]
attribute Setoid.LimType.atHom [coercion]

namespace Setoid

    definition LimSet {C : CatType} (F : C ⟶ SetoidCat) : SetoidType :=
        Setoid.MkOb
        (LimType F)
        ( λ(a b : LimType F), ∀(X : C), (a X) ≡(F X)≡ (b X))
        ( λ(a : LimType F), λ(X : C),
            @SetoidType.Refl (F X) (a X))
        ( λ(a b c : LimType F), λ ab bc, λ(X : C),
            @SetoidType.Trans (F X) (a X) (b X) (c X) (ab X) (bc X))
        ( λ(a b : LimType F), λ ab, λ(X : C),
            @SetoidType.Sym (F X) (a X) (b X) (ab X))
    check LimSet

namespace Lim

    definition onHom.onElEl {C : CatType} {F G : C ⟶ SetoidCat}
            : (F ⟹ G) → LimSet F → LimSet G
    := λ (nat : F ⟹ G), λ(lim : LimType F), Setoid.MkLim
            /- atOb -/ ( λ(X : C), (nat /$$ X) $ (lim X))
            /- atHom -/ ( λ(X Y : C), λ(m : X ⇒C⇒ Y),
                    ((nat /$$ Y) $/ (lim m)) ⊡(G Y)⊡ ((nat /$$/ m) /$ (lim X)))

    definition onHom.onElEqu {C : CatType} {F G : C ⟶ SetoidCat}
            : ∀(nat : F ⟹ G), ∀{a b : LimSet F}, (a ≡(LimSet F)≡ b) →
                ((onHom.onElEl nat a) ≡(LimSet G)≡ (onHom.onElEl nat b))
    := λ (nat : F ⟹ G), λ (a b : LimSet F), λ (eq : a ≡(LimSet F)≡ b),
            λ (X : C), (nat /$$ X) $/ (eq X)

    definition onHom.onEquEl {C : CatType} {F G : C ⟶ SetoidCat}
            : ∀{nat nat' : F ⟹ G}, ∀(eq : nat ≡(F ⟹ G)≡ nat'), ∀(a : LimSet F),
                ((onHom.onElEl nat a) ≡(LimSet G)≡ (onHom.onElEl nat' a))
    := λ (nat nat' : F ⟹ G), λ (eq : nat ≡(F ⟹ G)≡ nat'), λ (a : LimSet F),
            λ (X : C), (eq X) /$ (a X)

    definition onHom {C : CatType} {F G : C ⟶ SetoidCat}
        : (F ⟹ G) ⥤ (LimSet F ⥤ LimSet G)
    := Setoid.MkHom2 (F ⟹ G) (LimSet F) (LimSet G)
            onHom.onElEl (@onHom.onElEqu C F G) (@onHom.onEquEl C F G)

    definition OnId {C : CatType}
        : Functor.OnIdProp (C ⟶ SetoidCat) SetoidCat (@LimSet C) (@onHom C)
    := λ(F : C ⟶ SetoidCat), λ(lim : LimSet F), λ(X : C), ⊜

    definition OnMul {C : CatType}
        : Functor.OnMulProp (C ⟶ SetoidCat) SetoidCat (@LimSet C) (@onHom C)
    := λ(F G H : C ⟶ SetoidCat), λ(g : G ⟹ H), λ(f : F ⟹ G), λ(lim : LimSet F), λ(X : C), ⊜

    definition diagonal (C : CatType) (T : SetoidCat)
        : T ⥤ LimSet (Cat.Delta C SetoidCat T)
    := Setoid.MkHom
        /- onEl -/ ( λ(t : T), Setoid.MkLim
            /- atOb -/ ( λ(X : C), t)
            /- atHom -/ ( λ(X Y : C), λ(m : X ⇒C⇒ Y), ⊜))
        /- onEqu -/ ( λ(t1 t2 : T), λ(eq : t1 ≡(T)≡ t2), λ(X : C), eq)

    definition projection {C : CatType} (F : C ⟶ SetoidCat) (X : C)
        : LimSet F ⥤ (F X)
    := Setoid.MkHom
        /- onEl -/ ( λ(lim : Setoid.LimSet F), (lim X))
        /- onEqu -/ ( λ(lim lim': Setoid.LimSet F),
            λ(eq : lim ≡(Setoid.LimSet F)≡ lim'), eq X)
    print projection

    definition projection.cone {C : CatType} (F : C ⟶ SetoidCat)
        : Functor.IsConeProp (LimSet F) F (projection F)
    := λ(A B : C), λ(m : A ⇒C⇒ B), λ(lim : LimSet F), lim m
    print projection.cone

end Lim

    -- limit in SetoidCat
    definition Lim {C : CatType}
        : (C ⟶ SetoidCat) ⟶ SetoidCat
    := Functor.MkOb (@LimSet C) (@Lim.onHom C) (@Lim.OnId C) (@Lim.OnMul C)
    print Lim

    definition HasLim : HaveAllLim SetoidCat  :=
        λ(C : CatType), RightAdj.mk
            (@Setoid.Lim C)
            (AdjType.mk
                (Functor.MkHom
                /- onOb -/ ( Setoid.Lim.diagonal C )
                /- onHom -/ ( λ(T T' : SetoidCat), λ(f : T ⥤ T'), λ(t : T), ⊜))
                (Functor.MkHom
                /- onOb -/ ( λ F, Functor.NatFromCone (Lim.projection F) (@Lim.projection.cone C F))
                /- onHom -/ ( λ F1 F2, λ(f : F1 ⟹ F2), λ(X : C), λ(lim : LimSet F1), ⊜))
                ( λ(T : SetoidCat), λ(X : C), λ(t : T), ⊜ )
                ( λ(F : C⟶SetoidCat), λ(lim : LimSet F), λ(X : C), ⊜) )
    print HasLim

end Setoid

-- problem with levels in PREDICATIVE universe hierarchy
axiom BottomSet : SetoidType -- := Initial.FromLim Setoid.HasLim

noncomputable -- OK in IMPREDICATIVE universe hierarchy
definition BottomCat : CatType := Cat.FromSet BottomSet

end EXE
