/- Setoid.lean -/

set_option pp.universes true
set_option pp.metavar_args false

/-  naming conventions:
        - names with suffix Type are types,
            with suffix Prop are properties,
            with suffix Set are setoids,
            with suffix Cat are categories.
        - additional definitions for `MyType` are in `My` namespace
-/

namespace EXE

-- equivalence
definition EquType.{uEl} : Type.{uEl} → Type.{max uEl 1} := λ El, ∀ (l r : El), Prop

-- axioms of equivalence
definition Equ.ReflProp {El : Type} (Equ : EquType El) : Prop
    := ∀{e0 : El}, Equ e0 e0
definition Equ.TransProp {El : Type} (Equ : EquType El) : Prop
    := ∀{e1 e2 e3 : El}, Equ e1 e2 → Equ e2 e3 → Equ e1 e3
definition Equ.SymProp {El : Type} (Equ : EquType El) : Prop
    := ∀{e1 e2 : El}, Equ e1 e2 → Equ e2 e1

/-
 - Definition of the type of categories (CatType) and the category of setoids (SetoidCat)
 -/

-- the type of setoids, objects of the category `Setoid`
record SetoidType.{uEl} : Type.{uEl + 1} :=
    (El : Type.{uEl})
    (Equ : EquType El)
    (Refl : Equ.ReflProp Equ)
    (Trans : Equ.TransProp Equ)
    (Sym : Equ.SymProp Equ)
print SetoidType
abbreviation Setoid.MkOb := SetoidType.mk

-- carrier of setoid
attribute SetoidType.El [coercion]
notation ` [` S `] ` := SetoidType.El S        -- elements of `S`
notation a ` ≡` S `≡ ` b :10 := SetoidType.Equ S a b      -- `a=b` in `S`
notation ` ⊜ ` := SetoidType.Refl _
notation ab ` ⊡` S `⊡ ` bc :100 := SetoidType.Trans S ab bc

definition Equ.Closure {El : Type} : EquType El → EquType El :=
    λ (rel : EquType El),
    λ (e1 e2 : El),
    ∀ (equ : EquType El),
    ∀ (refl : Equ.ReflProp equ)(trans : Equ.TransProp equ)(sym : Equ.SymProp equ),
    ∀ (impl : ∀ (ee1 ee2 : El), rel ee1 ee2  → equ ee1 ee2),
    equ e1 e2

definition Setoid.Closure {El : Type} : EquType El → SetoidType :=
    λ (rel : EquType El),
    Setoid.MkOb
        El
        (Equ.Closure rel)
        ( λ (e0 : El),
            λ (equ : EquType El),
            λ (refl : Equ.ReflProp equ)(trans : Equ.TransProp equ)(sym : Equ.SymProp equ),
            λ (impl : ∀ (ee1 ee2 : El), rel ee1 ee2  → equ ee1 ee2),
            @refl e0)
        ( λ (e1 e2 e3 : El),
          λ (eq12 : Equ.Closure rel e1 e2),
          λ (eq23 : Equ.Closure rel e2 e3),
            λ (equ : EquType El),
            λ (refl : Equ.ReflProp equ)(trans : Equ.TransProp equ)(sym : Equ.SymProp equ),
            λ (impl : ∀ (ee1 ee2 : El), rel ee1 ee2  → equ ee1 ee2),
            @trans e1 e2 e3
                (eq12 equ @refl @trans @sym impl)
                (eq23 equ @refl @trans @sym impl))
        ( λ (e1 e2 : El),
          λ (eq12 : Equ.Closure rel e1 e2),
            λ (equ : EquType El),
            λ (refl : Equ.ReflProp equ)(trans : Equ.TransProp equ)(sym : Equ.SymProp equ),
            λ (impl : ∀ (ee1 ee2 : El), rel ee1 ee2  → equ ee1 ee2),
            @sym e1 e2
                (eq12 equ @refl @trans @sym impl))

-- util

definition Equ.Trans3Prop {El : Type} (Equ : EquType El) : Prop
    := ∀{e1 e2 e3 e4 : El}, Equ e1 e2 → Equ e2 e3 → Equ e3 e4 → Equ e1 e4

definition SetoidType.Trans3 (S : SetoidType) : Equ.Trans3Prop (SetoidType.Equ S)
    := λ e1 e2 e3 e4, λ eq12 eq23 eq34, eq12 ⊡S⊡ eq23 ⊡S⊡ eq34

-- morphisms in the category `Setoid`
namespace Setoid
    record HomType (A : SetoidType) (B : SetoidType)
        : Type :=
        (onEl : Π(a : A), B)
        (onEqu : ∀{a1 a2 : A}, (a1 ≡_≡ a2) → (onEl a1 ≡_≡ onEl a2))
    print HomType
    abbreviation MkHom {A B : SetoidType} := @Setoid.HomType.mk A B
end Setoid

-- action on carrier
attribute Setoid.HomType.onEl [coercion]
infixl ` $ `:100 := Setoid.HomType.onEl
attribute Setoid.HomType.onEqu [coercion]
infixl ` $/ `:100 := Setoid.HomType.onEqu

-- hom-sets in `Setoid` category
definition Setoid.HomSet (A B : SetoidType) : SetoidType :=
    Setoid.MkOb
    /- El-/ (Setoid.HomType A B)
    /- Equ-/ ( λ(f g : Setoid.HomType A B), ∀(a : A), (f a) ≡_≡ (g a))
    /- Refl-/ ( λ f, λ a, ⊜)
    /- Trans -/ ( λ f g h, λ fg gh, λ a, (fg a) ⊡_⊡ (gh a))
    /- Sym -/ ( λ f g, λ fg, λ a, SetoidType.Sym B (fg a))

definition app_set_hom_equ {A B : SetoidType} {f g : Setoid.HomType A B}
    (eq : ∀(a : A), (f a) ≡_≡ (g a)) (a : A) : (f a) ≡_≡ (g a)
:= eq a

infixl ` /$ `:100 := app_set_hom_equ

-- the dedicated arrow for morphisms of setoids
infixr ` ⥤ `:10 := Setoid.HomSet

namespace Setoid

    abbreviation MkHom2 (A B C : SetoidType)
        (onElEl : ∀(a : A), ∀(b : B), [C])
        (onElEqu : ∀(a : A), ∀{b1 b2 : B}, ∀(e : b1 ≡_≡ b2), (onElEl a b1 ≡_≡ onElEl a b2))
        (onEquEl : ∀{a1 a2 : A}, ∀(e : a1 ≡_≡ a2), ∀(b : B), (onElEl a1 b ≡_≡ onElEl a2 b))
        : A ⥤ B ⥤ C :=
        @MkHom A (B ⥤ C)
            ( λ (a : A), @MkHom B C (@onElEl a) (@onElEqu a))
            @onEquEl

    definition Mul.onElEl {A B C : SetoidType} (f : B ⥤ C) (g : A ⥤ B) : A ⥤ C :=
        MkHom
            ( λ (a : A), f $ (g $ a))
            ( λ (a1 a2 : A), λ(a12 : a1 ≡_≡ a2), f $/ (g $/ a12) )
    definition HomEquProp {A B : SetoidType} (f g : A ⥤ B) : Prop := f ≡(A ⥤ B)≡ g

    definition SubSingletonProp (S : SetoidType) : Prop := ∀(a b : S), a ≡_≡ b

    record SingletonType (S : SetoidType) : Type :=
        (center : S)
        (connect : ∀(s : S), center ≡S≡ s)

    lemma SingletonIsSub {S : SetoidType} (ok : SingletonType S) : SubSingletonProp S :=
        let okk := SingletonType.connect ok in
        λ a b, (SetoidType.Sym _ (okk a)) ⊡_⊡ ((okk b))

    abbreviation MkSingleton {S : SetoidType} := @SingletonType.mk S

    definition FromType (T : Type) : SetoidType :=
        Setoid.MkOb
            T
            ( λ x y, true)
            ( λ x, true.intro)
            ( λ x y z, λ xy yz, true.intro)
            ( λ x y, λ xy, true.intro)

    definition FromType.Singleton (T : Type) : Setoid.SubSingletonProp (FromType T) :=
        λ x y, true.intro

    definition FromMap {T1 T2 : Type} (f : T1 → T2)
        : (FromType T1) ⥤ (FromType T2) :=
        Setoid.MkHom f (λ x y, λ xy, true.intro)

    definition Const (X Y : SetoidType) (y : Y) : X ⥤ Y := Setoid.MkHom
        ( λ x, y )
        ( λ x1 x2, λ e12, ⊜ )

end Setoid

infixl `∙`: 100 := Setoid.Mul.onElEl
infix `⥰`: 10 := Setoid.HomEquProp

definition Prop.id {P : Prop} : (P → P) := λp, p

definition Prop.mul {P Q R : Prop} (pq : P → Q) (qr : Q → R)
    : (P → R) := λp, qr (pq p)

definition PropSet : SetoidType :=
    Setoid.MkOb
    /- El -/ (Prop)
    /- Equ -/ ( λ(P Q : Prop), and (P → Q) (Q → P))
    /- Refl -/ ( λ P, and.intro Prop.id Prop.id)
    /- Trans -/ ( λ P Q R, λ pq qr, and.intro
        (Prop.mul (and.left pq) (and.left qr)) (Prop.mul (and.right qr) (and.right pq)))
    /- Sym -/ ( λ P Q, λ pq, and.intro (and.right pq) (and.left pq))

-- TODO: Hom (pro)functor;
-- TODO: Sigma: (B→Type) → (E→B), UnSigma: (E→B) → (B→Type)
-- TODO: initial as limit, comma categories, cats of algebras
-- TODO: adjunctions: by isomorphism of profunctors
-- TODO: TT-like recursor, induction

-- TODO: Id(Refl) coincide with SetoidType.Equ
end EXE
