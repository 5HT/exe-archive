/- Pi.lean -/

namespace EXE

universe variable a
universe variable b
constant ImperdicativePi : Π {A : Type.{a}}, (A → Type.{b}) → Type.{b}

abbreviation ℿ {A : Type.{a}} := @ImperdicativePi A

definition Sigma := λ (A: Type),
                    λ (B: A -> Type),
                    ∀ (DepProd: Type),
                    ∀ (Make: (∀ (_1: A), ∀ (_2: B _1), DepProd) ),
                    DepProd

definition snd := λ (A: Type),
                  λ (B: A -> Type),
                  λ (_1: A),
                  λ (_2: B _1),
                  λ (P: Sigma A B),
                  _2

print snd

end EXE
