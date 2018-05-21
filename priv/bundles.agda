
  image :
    ∀ {i j} {A : U i} {B : U j}
    → (f : A → B) → U (i ⊔ j)
  image f = ∑ (λ b → the-image-of f contains b)

  _*_ : ∀ {E B B′ : 𝒰₀}
    → (f : B′ → B) → (φ : E → B) → 𝒰₀
  f * φ = upper-left-vertex-of (complete-to-pullback-square φ f)

  complete-to-pullback-square :
    ∀ {A B C : 𝒰₀} (f : A → C) (g : B → C)
    → pullback-square f g (p₁-of-pullback f g) (p₂-of-pullback f g)
  complete-to-pullback-square f g =
    let step1 : id left-inverse-of induced-map-to-pullback (p₁-of-pullback _ _) (p₂-of-pullback _ _) p-homotopy
        step1  = λ {(a and b are-in-the-same-fiber-by γ) → refl}
        step2 : id right-inverse-of induced-map-to-pullback
                 (p₁-of-pullback _ _) (p₂-of-pullback _ _) p-homotopy
        step2 = λ {(a and b are-in-the-same-fiber-by γ) → refl}
    in the-square-commuting-by p-homotopy and-inducing-an-equivalence-by
      (has-left-inverse id by step1 and-right-inverse id by step2)

  upper-left-vertex-of :
    ∀ {Z A B C : 𝒰₀}
      {f : A → C}  {g : B → C}
      {z₁ : Z → A} {z₂ : Z → B}
    → pullback-square f g z₁ z₂
    → 𝒰₀
  upper-left-vertex-of {Z} {_} {_} {_} {_} {_} {_} {_} _ = Z

  the-image-of_contains :
    ∀ {i j} {A : U i} {B : U j}
    → (f : A → B) → (B → U (i ⊔ j))
  the-image-of f contains b = ∥ ∑ (λ a → f(a) ≈ b) ∥

  record pullback-square {i} {Z A B C : U i} (f : A → C)  (g : B → C)
                                      (z₁ : Z → A) (z₂ : Z → B)  : U i where
    constructor the-square-commuting-by_and-inducing-an-equivalence-by_
    field
      γ : f ∘ z₁ ⇒ g ∘ z₂
      proof : (induced-map-to-pullback {f = f} {g = g}  z₁ z₂ γ) is-an-equivalence

  induced-map-to-pullback :
    ∀ {i} {Z A B C : U i} {f : A → C} {g : B → C}
    → (z₁ : Z → A) → (z₂ : Z → B) → (γ : f ∘ z₁ ⇒ g ∘ z₂)
    → (Z → pullback f g)
  induced-map-to-pullback z₁ z₂ γ z =
    (z₁ z) and (z₂ z) are-in-the-same-fiber-by γ z 

  data 𝟙 : 𝒰₀ where
    ∗ : 𝟙

  BAut : (A : 𝒰₀) → U₁
  BAut A = image {_} {_} {𝟙} {𝒰₀} (λ ∗ → A)

