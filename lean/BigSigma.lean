/- BigSigma.lean -/

import Setoid
import Cat
import Functor
import Over
import DepSet

namespace EXE

record BigSigmaType {Base : SetoidType} (P : DepSetoidCat Base) : Type :=
    (base : Base)
    (fiber : [P $$ base] )

definition BigSigma.Equ {Base : SetoidType} (P : DepSetoidCat Base)
        : EquType (BigSigmaType P) :=
    λ (a b : BigSigmaType P),
    (∃ eqbase : BigSigmaType.base a ≡Base≡ BigSigmaType.base b,
    (((P $$/ eqbase) $ (BigSigmaType.fiber a)) ≡(P $$ (BigSigmaType.base b))≡ (BigSigmaType.fiber b)))

definition BigSigma.Equ.base {Base : SetoidType} {P : DepSetoidCat Base}
    {a b : BigSigmaType P} (eq : BigSigma.Equ P a b)
        : BigSigmaType.base a ≡Base≡ BigSigmaType.base b :=
    exists.elim eq (λ eqbase eqfiber, eqbase)

definition BigSigma.Equ.fiber {Base : SetoidType} {P : DepSetoidCat Base}
    {a b : BigSigmaType P} (eq : BigSigma.Equ P a b)
        : (((P $$/ (BigSigma.Equ.base eq)) $ (BigSigmaType.fiber a)) ≡(P $$ (BigSigmaType.base b))≡ (BigSigmaType.fiber b)) :=
    exists.elim eq (λ eqbase eqfiber, eqfiber)

definition BigSigma.Refl {Base : SetoidType} (P : DepSetoidCat Base)
        : Equ.ReflProp (BigSigma.Equ P) :=
    λ (s0 : BigSigmaType P),
    exists.intro
        (@SetoidType.Refl Base (BigSigmaType.base s0))
        ((FunctorType.onId P) /$ (BigSigmaType.fiber s0))

definition BigSigma.Trans {Base : SetoidType} (P : DepSetoidCat Base)
        : Equ.TransProp (BigSigma.Equ P) :=
    λ (s1 s2 s3 : BigSigmaType P),
    λ (eq12 : BigSigma.Equ P s1 s2), λ (eq23 : BigSigma.Equ P s2 s3),
    exists.intro
        (@SetoidType.Trans Base
            (BigSigmaType.base s1)
            (BigSigmaType.base s2)
            (BigSigmaType.base s3)
            (BigSigma.Equ.base eq12)
            (BigSigma.Equ.base eq23))
        (@SetoidType.Trans3 (P $$ (BigSigmaType.base s3))
            ((P $$/ (@SetoidType.Trans Base
                        (BigSigmaType.base s1)
                        (BigSigmaType.base s2)
                        (BigSigmaType.base s3)
                        (BigSigma.Equ.base eq12)
                        (BigSigma.Equ.base eq23))) $
                (BigSigmaType.fiber s1))
            ((P $$/ (BigSigma.Equ.base eq23)) $
                ((P $$/ (BigSigma.Equ.base eq12)) $
                    (BigSigmaType.fiber s1)))
            ((P $$/ (BigSigma.Equ.base eq23)) $
                (BigSigmaType.fiber s2))
            (BigSigmaType.fiber s3)
            ((FunctorType.onMul P (BigSigma.Equ.base eq23) (BigSigma.Equ.base eq12)) /$
                (BigSigmaType.fiber s1))
            ((P $$/ (BigSigma.Equ.base eq23)) $/
                (BigSigma.Equ.fiber eq12))
            (BigSigma.Equ.fiber eq23))

definition BigSigma.Sym {Base : SetoidType} (P : DepSetoidCat Base)
        : Equ.SymProp (BigSigma.Equ P) :=
    λ (s1 s2 : BigSigmaType P),
    λ (eq12 : BigSigma.Equ P s1 s2),
    exists.intro
        (SetoidType.Sym Base (BigSigma.Equ.base eq12))
        (SetoidType.Sym
            (P $$ (BigSigmaType.base s1))
            (DepAdj P
                (BigSigma.Equ.base eq12)
                (SetoidType.Sym Base (BigSigma.Equ.base eq12))
                (BigSigmaType.fiber s1)
                (BigSigmaType.fiber s2)
                (BigSigma.Equ.fiber eq12)))

definition BigSigmaSet {Base : SetoidType} (P : DepSetoidCat Base)
    : SetoidType :=
    Setoid.MkOb
    /- El-/ (BigSigmaType P)
    /- Equ-/ (BigSigma.Equ P)
    /- Refl-/ (@BigSigma.Refl Base P)
    /- Trans -/ (@BigSigma.Trans Base P)
    /- Sym -/ (@BigSigma.Sym Base P)

definition BigSigmaFirst {Base : SetoidType} (P : DepSetoidCat Base)
        : BigSigmaSet P ⥤ Base :=
    Setoid.MkHom
        (BigSigmaType.base)
        (@BigSigma.Equ.base Base P)

definition BigSigma.onOb (Base : SetoidType)
        : DepSetoidCat Base → OverSetoidCat Base :=
    λ P, OverType.mk
    /- Dom -/ (BigSigmaSet P)
    /- Hom -/ (BigSigmaFirst P)

definition help1
    (Base : SetoidType)
    (X Y : DepSetoidCat Base)
    (m : X ⟹ Y)
    (s1 s2 : BigSigmaSet X)
    (eq : s1 ≡(BigSigmaSet X)≡ s2)
        : ((Y $$/ (BigSigma.Equ.base eq)) ∙ ((m /$$ (BigSigmaType.base s1))))
            ≡((X $$ (BigSigmaType.base s1)) ⥤ (Y $$ (BigSigmaType.base s2)))≡
          ((m /$$ (BigSigmaType.base s2)) ∙ ((X $$/ (BigSigma.Equ.base eq))))
    := m /$/$/ (BigSigma.Equ.base eq)

definition help11
    (Base : SetoidType)
    (X Y : DepSetoidCat Base)
    (m : X ⟹ Y)
    (s1 s2 : BigSigmaSet X)
    (eq : s1 ≡(BigSigmaSet X)≡ s2)
        : ((Y $$/ (BigSigma.Equ.base eq)) $ ((m /$$ (BigSigmaType.base s1)) $ (BigSigmaType.fiber s1)))
            ≡(Y $$ (BigSigmaType.base s2))≡
          ((m /$$ (BigSigmaType.base s2)) $ ((X $$/ (BigSigma.Equ.base eq)) $ (BigSigmaType.fiber s1)))
    := (help1 Base X Y m s1 s2 eq) /$ (BigSigmaType.fiber s1)

definition help2
    (Base : SetoidType)
    (X Y : DepSetoidCat Base)
    (m : X ⟹ Y)
    (s1 s2 : BigSigmaSet X)
    (eq : s1 ≡(BigSigmaSet X)≡ s2)
        : ((m /$$ (BigSigmaType.base s2)) $ ((X $$/ (BigSigma.Equ.base eq)) $ (BigSigmaType.fiber s1)))
            ≡(Y $$ (BigSigmaType.base s2))≡
          ((m /$$ (BigSigmaType.base s2)) $ (BigSigmaType.fiber s2))
    := proof ((m /$$ (BigSigmaType.base s2)) $/ (BigSigma.Equ.fiber eq)) qed

definition BigSigma.onHom.onEl.onDom.onEl (Base : SetoidType) (X Y : DepSetoidCat Base)
    (m : X ⇒(DepSetoidCat Base)⇒ Y)
        : BigSigmaType X → BigSigmaType Y :=
    λ (s : BigSigmaSet X),
        BigSigmaType.mk
            (BigSigmaType.base s)
            ((m /$$ (BigSigmaType.base s)) $ (BigSigmaType.fiber s))

definition BigSigma.onHom.onEl.onDom (Base : SetoidType) (X Y : DepSetoidCat Base)
    (m : X ⇒(DepSetoidCat Base)⇒ Y)
        : BigSigmaSet X ⥤ BigSigmaSet Y :=
    @Setoid.MkHom (BigSigmaSet X) (BigSigmaSet Y)
        ( BigSigma.onHom.onEl.onDom.onEl Base X Y m)
        ( λ (s1 s2 : BigSigmaSet X),
          λ (eq : s1 ≡(BigSigmaSet X)≡ s2),
          proof
              exists.intro
                (BigSigma.Equ.base eq)
                (   (help11 Base X Y m s1 s2 eq)
                        ⊡(Y $$ (BigSigmaType.base s2))⊡
                    (help2 Base X Y m s1 s2 eq))
          qed)

definition help3 (Base : SetoidType) (X Y : DepSetoidCat Base)
    (s : BigSigmaSet X)
        : (Y $$/ (@SetoidType.Refl Base (BigSigmaType.base s)))
            ≡((Y $$ (BigSigmaType.base s)) ⥤ (Y $$ (BigSigmaType.base s)))≡
          (@Setoid.Id (Y $$ (BigSigmaType.base s))) :=
    FunctorType.onId Y

definition help31 (Base : SetoidType) (X Y : DepSetoidCat Base)
    (m1 m2 : X ⟹ Y) (eq : m1 ≣ m2) (s : BigSigmaSet X)
        : ((Y $$/ (@SetoidType.Refl Base (BigSigmaType.base s))) $
                ((m1 /$$ (BigSigmaType.base s)) $ (BigSigmaType.fiber s)))
            ≡(Y $$ (BigSigmaType.base s))≡
          ((@Setoid.Id (Y $$ (BigSigmaType.base s))) $
                ((m1 /$$ (BigSigmaType.base s)) $ (BigSigmaType.fiber s))) :=
    proof
        (help3 Base X Y s) /$
            ((m1 /$$ (BigSigmaType.base s)) $ (BigSigmaType.fiber s))
    qed

definition help32 (Base : SetoidType) (X Y : DepSetoidCat Base)
    (m1 m2 : X ⟹ Y) (eq : m1 ≣ m2) (s : BigSigmaSet X)
        : ((Y $$/ (@SetoidType.Refl Base (BigSigmaType.base s))) $
                ((m1 /$$ (BigSigmaType.base s)) $ (BigSigmaType.fiber s)))
            ≡(Y $$ (BigSigmaType.base s))≡
          (     ((m1 /$$ (BigSigmaType.base s)) $ (BigSigmaType.fiber s))) :=
    proof
        (help31 Base X Y m1 m2 eq s)
    qed

definition help4 (Base : SetoidType) (X Y : DepSetoidCat Base)
    (m1 m2 : X ⟹ Y) (eq : m1 ≣ m2) (s : BigSigmaSet X)
        : (m1 /$$ (BigSigmaType.base s))
            ≡((X $$ (BigSigmaType.base s)) ⥤ (Y $$ (BigSigmaType.base s)))≡
          (m2 /$$ (BigSigmaType.base s)) :=
    (eq (BigSigmaType.base s))

definition help41 (Base : SetoidType) (X Y : DepSetoidCat Base)
    (m1 m2 : X ⟹ Y) (eq : m1 ≣ m2) (s : BigSigmaSet X)
        : ((m1 /$$ (BigSigmaType.base s)) $ (BigSigmaType.fiber s))
            ≡(Y $$ (BigSigmaType.base s))≡
          ((m2 /$$ (BigSigmaType.base s)) $ (BigSigmaType.fiber s)) :=
    ((help4 Base X Y m1 m2 eq s) /$ (BigSigmaType.fiber s))

definition BigSigma.onHom.onEqu.onDom.onEl (Base : SetoidType) (X Y : DepSetoidCat Base)
    (m1 m2 : X ⟹ Y) (eq : m1 ≣ m2) (s : BigSigmaSet X)
        : (BigSigma.onHom.onEl.onDom.onEl Base X Y m1 s)
            ≡(BigSigmaSet Y)≡
          (BigSigma.onHom.onEl.onDom.onEl Base X Y m2 s) :=
    proof exists.intro
        (@SetoidType.Refl Base (BigSigmaType.base s))
        (   (help32 Base X Y m1 m2 eq s)
                ⊡(Y $$ (BigSigmaType.base s))⊡
            (help41 Base X Y m1 m2 eq s))
    qed

definition BigSigma.onHom.onEl (Base : SetoidType) (X Y : DepSetoidCat Base)
    (m : X ⇒(DepSetoidCat Base)⇒ Y)
        : (BigSigma.onOb Base X ⇒(OverSetoidCat Base)⇒ BigSigma.onOb Base Y) :=
    @Over.HomType.mk SetoidCat Base (BigSigma.onOb Base X) (BigSigma.onOb Base Y)
        ( BigSigma.onHom.onEl.onDom Base X Y m)
        ( λ (s : BigSigmaSet X), @SetoidType.Refl Base (BigSigmaType.base s))

definition BigSigma.onHom.onEqu.onDom (Base : SetoidType) (X Y : DepSetoidCat Base)
    (m1 m2 : X ⟹ Y) (eq : m1 ≣ m2)
        : (BigSigma.onHom.onEl.onDom Base X Y m1)
            ≡(BigSigmaSet X ⥤ BigSigmaSet Y)≡
          (BigSigma.onHom.onEl.onDom Base X Y m2) :=
    BigSigma.onHom.onEqu.onDom.onEl Base X Y m1 m2 eq

definition BigSigma.onHom.onEqu (Base : SetoidType) (X Y : DepSetoidCat Base)
    (m1 m2 : X ⟹ Y) (eq : m1 ≣ m2)
        : (BigSigma.onHom.onEl Base X Y m1)
            ≡(BigSigma.onOb Base X ⇒(OverSetoidCat Base)⇒ BigSigma.onOb Base Y)≡
          (BigSigma.onHom.onEl Base X Y m2) :=
    BigSigma.onHom.onEqu.onDom Base X Y m1 m2 eq

definition BigSigma.onHom (Base : SetoidType) (X Y : DepSetoidCat Base)
        : (X ⇒(DepSetoidCat Base)⇒ Y) ⥤
          (BigSigma.onOb Base X ⇒(OverSetoidCat Base)⇒ BigSigma.onOb Base Y) :=
    Setoid.MkHom
        ( BigSigma.onHom.onEl Base X Y)
        ( BigSigma.onHom.onEqu Base X Y)

definition BigSigmaFunctor (Base : SetoidType)
        : DepSetoidCat Base ⟶ OverSetoidCat Base :=
    Functor.MkOb
    /- onOb -/ ( λ(X : DepSetoidCat Base), BigSigma.onOb Base X)
    /- onHom -/ ( λ(X Y : DepSetoidCat Base), BigSigma.onHom Base X Y)
    /- onId -/ ( λ(X : DepSetoidCat Base), sorry)
    --  BigSigma.onHom.onEl.onDom.onEl Base P P (@Functor.Id _ _ P) == @Setoid.Id (BigSigmaType P)
    /- onMul -/ ( λ(X Y Z : DepSetoidCat Base), λ(g : Y ⇒_⇒ Z), λ(f : X ⇒_⇒ Y), sorry)
    -- 

end EXE
