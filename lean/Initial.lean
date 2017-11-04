/- Initial -/

import Setoid
import Cat
import Mor
import Functor
import Adjunction

set_option pp.universes true
set_option pp.metavar_args false

namespace EXE

record InitialType (C : CatType) (Obj : C) : Type :=
    (Cone : Functor.ConeType Obj 𝟙)
    (IsCone : Functor.IsConeProp Obj 𝟙 Cone)
    (Ok : Cone Obj ≡(Obj ⇒C⇒ Obj)≡ ①)
-- print InitialType

namespace Initial

    abbreviation Mk {C : CatType} {Obj : C} :=
        @InitialType.mk C Obj

    lemma Singleton {C : CatType} {I : C}
        (init : InitialType C I) (X : C)
            : Setoid.SingletonType (I ⇒C⇒ X) :=
        Setoid.MkSingleton
            ( InitialType.Cone init X)
            ( λ(f : I ⇒C⇒ X),
                (InitialType.IsCone init f) ⊡_⊡
                (f ⊙C⊙/ (InitialType.Ok init)) ⊡_⊡
                (CatType.UnitR C f) )

    definition FromLim.{o1 h1} {C : CatType.{o1 h1}}
        (lim : HaveAllLim.{o1 h1 o1 h1} C)
            : C :=
        Lim.Apply lim 𝟙
    print FromLim

    definition FromLim.Ok.{o1 h1} {C : CatType.{o1 h1}}
        (lim : HaveAllLim.{o1 h1 o1 h1} C)
            : InitialType C ((RightAdj.Right (lim C)) $$ 𝟙) := -- (Lim.Apply lim 𝟙)
        proof Mk
            ( λ X, ((AdjType.counit (RightAdj.adj (lim C))) /$$ 𝟙) /$$ X)
            ( λ A B, λ(m : A ⇒C⇒ B),
                    (CatType.UnitRInv C ((((AdjType.counit (RightAdj.adj (lim C))) /$$ 𝟙)) /$$ B))
                        ⊡(((RightAdj.Right (lim C)) $$ 𝟙) ⇒C⇒ B)⊡
                    (((AdjType.counit (RightAdj.adj (lim C))) /$$ 𝟙) /$$/ m) )
        begin
            have eqNat :
                    (((AdjType.counit (RightAdj.adj (lim C))) /$$ 𝟙) ⊙(C⟶C)⊙
                    ((Cat.Delta C C) $$/
                        (((AdjType.counit (RightAdj.adj (lim C))) /$$ 𝟙) /$$ ((RightAdj.Right (lim C)) $$ 𝟙))))
                            ≡((Cat.Delta C C ((RightAdj.Right (lim C)) $$ 𝟙)) ⟹ 𝟙)≡
                    (((AdjType.counit (RightAdj.adj (lim C))) /$$ 𝟙) ⊙(C⟶C)⊙
                        ①),
                from λ(X : C), proof
                    SetoidType.Sym (((RightAdj.Right (lim C)) $$ 𝟙) ⇒C⇒ X)
                    (((AdjType.counit (RightAdj.adj (lim C))) /$$ 𝟙) /$$/
                    (((AdjType.counit (RightAdj.adj (lim C))) /$$ 𝟙) /$$ X))
                qed,
            have myCong : ∀ {g1 g2 : (Cat.Delta C C ((RightAdj.Right (lim C)) $$ 𝟙)) ⟹ 𝟙},
                    ∀ (eq : g1 ≡((Cat.Delta C C ((RightAdj.Right (lim C)) $$ 𝟙)) ⟹ 𝟙)≡ g2),
                    ( (((RightAdj.Right (lim C)) $$/ g1) ⊙C⊙
                            ((AdjType.unit (RightAdj.adj (lim C))) /$$ ((RightAdj.Right (lim C)) $$ 𝟙)))
                        ≡(((RightAdj.Right (lim C)) $$ 𝟙) ⇒C⇒ ((RightAdj.Right (lim C)) $$ 𝟙))≡
                    (((RightAdj.Right (lim C)) $$/ g2) ⊙C⊙
                            ((AdjType.unit (RightAdj.adj (lim C))) /$$ ((RightAdj.Right (lim C)) $$ 𝟙))) ),
                from
                    λ (g1 g2 : (Cat.Delta C C ((RightAdj.Right (lim C)) $$ 𝟙)) ⟹ 𝟙),
                    λ (eq : g1 ≡((Cat.Delta C C ((RightAdj.Right (lim C)) $$ 𝟙)) ⟹ 𝟙)≡ g2),
                proof
                    ((RightAdj.Right (lim C)) $$// eq) /⊙C⊙
                            ((AdjType.unit (RightAdj.adj (lim C))) /$$ ((RightAdj.Right (lim C)) $$ 𝟙))
                qed,
            show (
                (((AdjType.counit (RightAdj.adj (lim C))) /$$ 𝟙) /$$ ((RightAdj.Right (lim C)) $$ 𝟙))
                        ≡(((RightAdj.Right (lim C)) $$ 𝟙) ⇒C⇒ ((RightAdj.Right (lim C)) $$ 𝟙))≡
                    ①),
                from proof
                    (SetoidType.Sym (((RightAdj.Right (lim C)) $$ 𝟙) ⇒C⇒ ((RightAdj.Right (lim C)) $$ 𝟙))
                    ((Adj.IsoOnLR.ReqR (RightAdj.adj (lim C))
                        ((RightAdj.Right (lim C)) $$ 𝟙) 𝟙)
                    (((AdjType.counit (RightAdj.adj (lim C))) /$$ 𝟙) /$$ ((RightAdj.Right (lim C)) $$ 𝟙)))
                    )
                        ⊡(((RightAdj.Right (lim C)) $$ 𝟙) ⇒C⇒ ((RightAdj.Right (lim C)) $$ 𝟙))⊡
                    (myCong eqNat)
                        ⊡(((RightAdj.Right (lim C)) $$ 𝟙) ⇒C⇒ ((RightAdj.Right (lim C)) $$ 𝟙))⊡
                    ((Adj.IsoOnLR.ReqR (RightAdj.adj (lim C))
                        ((RightAdj.Right (lim C)) $$ 𝟙) 𝟙)
                    ①)
                qed
        end
        qed

end Initial

record TerminalType (C : CatType) (Obj : C) : Type :=
    (Cocone : Functor.CoconeType 𝟙 Obj)
    (IsCocone : Functor.IsCoconeProp 𝟙 Obj Cocone)
    (Ok : Cocone Obj ≡(Obj ⇒C⇒ Obj)≡ ①)

namespace Terminal

    abbreviation Mk {C : CatType} {Obj : C} :=
        @TerminalType.mk C Obj

    lemma Singleton {C : CatType} {T : C} (term : TerminalType C T) (X : C)
            : Setoid.SingletonType (X ⇒C⇒ T) :=
        Setoid.MkSingleton
            ( TerminalType.Cocone term X)
            ( λ(g : X ⇒C⇒ T),
                (SetoidType.Sym _ (TerminalType.IsCocone term g)) ⊡_⊡
                ((TerminalType.Ok term) /⊙C⊙ g) ⊡_⊡
                (CatType.UnitL C g))

    definition FromColim.{o1 h1} {C : CatType.{o1 h1}}
        (colim : HaveAllColim.{o1 h1 o1 h1} C)
        : C
    := Colim.Apply colim 𝟙

/- -- actually, we do not need it now
    definition FromColim.Ok {C : CatType.{o1 h1}}
        (colim : HaveAllColim.{o1 h1 o1 h1} C)
        : TerminalType C (FromColim colim)
    := sorry
-/

-- TODO: limit of empty is terminal (actually need it!)

end Terminal

end EXE
