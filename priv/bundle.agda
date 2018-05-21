
-- (с) @felixwellen

-- TOC:

-- Path Types
-- Etale Maps
-- Abstract homogeneous structure
-- Shape (fundamental infinity-Groupoid)
-- Manifold
-- Surjections
-- Image
-- Unit
-- Automorphism
-- G-sets (Covering Spaces)

-- Path types

  infixl 60 _$≃_
  _$≃_ : ∀ {i} {j} {A : U i} {B : 𝒰 j} → (f : A ≃ B) → A → B
  (f is-an-equivalence-because _) $≃ a = f a

  record _≃_  {i j} (A : U i) (B : U j) : U (i ⊔ j) where
    constructor _is-an-equivalence-because_
    field
      the-equivalence : A → B
      proof-of-invertibility : the-equivalence is-an-equivalence

-- Etale Maps

  -- X --→ ℑ X
  -- |      |
  -- f      ℑf
  -- ↓      ↓
  -- Y --→ ℑ Y

  _is-an-étale-map : ∀ {X Y : 𝒰₀} (f : X → Y) → 𝒰₀
  f is-an-étale-map =
    the-square-with-right (apply-ℑ-to-map f)
      bottom ℑ-unit
      top ℑ-unit
      left f
      commuting-by (naturality-of-ℑ-unit f)
     is-a-pullback-square

  underlying-map-of :
    ∀ {A B : 𝒰₀}
    → (A ─ét→ B) → (A → B)
  underlying-map-of (f , _) = f

  _ét→ :
    ∀ {A B : 𝒰₀}
    → (A ─ét→ B) → (A → B)
  f ét→ = underlying-map-of f

  _$ét_ :
    ∀ {A B : 𝒰₀}
    → (A ─ét→ B) → A → B
  f $ét x = (f ét→) x

-- Abstract homogeneous structure

  postulate
    𝔸 : 𝒰₀
    𝔸′ : homogeneous-structure-on 𝔸
    𝔸-nullfies-discrete-types :
      ∀ (A :{♭} 𝒰₀)
      → A is-crisply-discrete ≃ const {𝔸} {A} is-an-equivalence

  origin-of-𝔸 : 𝔸
  origin-of-𝔸 =
    let
      open homogeneous-structure-on_ 𝔸′
    in e

  record homogeneous-structure-on_ (A : 𝒰₀) : 𝒰₀ where
    field
      e : A
      ψ : (x : A) → (A ≃ A)
      is-translation-to : (x : A) → ((ψ x) $≃ e) ≈ x

-- Shape (fundamental infinity Grpoupoid)

  private
    data #ʃ (A : 𝒰₀) : 𝒰₀ where
      #σ : A → #ʃ A
      #κ  : (𝔸 → #ʃ A) → #ʃ A
      #κ′ : (𝔸 → #ʃ A) → #ʃ A

-- Manifold

  record _-manifold {V′ : 𝒰₀} (V : homogeneous-structure-on V′) : 𝒰₁ where
    field
      M : 𝒰₀
      W : 𝒰₀
      w : W ─ét→ M
      w-covers : (w ét→) is-surjective
      v : W ─ét→ V′

-- Surjections

  _is-surjective :
    ∀ {i} {j} {A : U i} {B : U j}
    → (A → B) → U (i ⊔ j)
  _is-surjective {_} {_} {A} {B} f = (b : B) → ∥ fiber-of f at b ∥

  record _↠_ {i} {j} (A : U i) (B : U j) : U (i ⊔ j) where
    constructor _is-surjective-by_
    field
      morphism : A → B
      proof-that-it-is-surjective : morphism is-surjective

  underlying-map-of-the-surjection :
    ∀ {i} {j} {A : U i} {B : U j}
    → (f : A ↠ B) → (A → B)
  underlying-map-of-the-surjection
    (morphism is-surjective-by proof-that-it-is-surjective) = morphism

  _$↠_ : ∀ {A B : 𝒰₀}
    → (f : A ↠ B) → A → B
  f $↠ a = (underlying-map-of-the-surjection f) a

-- Image

  the-image-of_contains :
    ∀ {i j} {A : U i} {B : U j}
    → (f : A → B) → (B → U (i ⊔ j))
  the-image-of f contains b = ∥ ∑ (λ a → f(a) ≈ b) ∥

  image :
    ∀ {i j} {A : U i} {B : U j}
    → (f : A → B) → U (i ⊔ j)
  image f = ∑ (λ b → the-image-of f contains b)

  _*_ : ∀ {E B B′ : 𝒰₀}
    → (f : B′ → B) → (φ : E → B) → 𝒰₀
  f * φ = upper-left-vertex-of (complete-to-pullback-square φ f)

  upper-left-vertex-of :
    ∀ {Z A B C : 𝒰₀}
      {f : A → C}  {g : B → C}
      {z₁ : Z → A} {z₂ : Z → B}
    → pullback-square f g z₁ z₂
    → 𝒰₀
  upper-left-vertex-of {Z} {_} {_} {_} {_} {_} {_} {_} _ = Z

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

-- Unit

  data 𝟙 : 𝒰₀ where
    ∗ : 𝟙

-- Automorphism

  BAut : (A : 𝒰₀) → U₁
  BAut A = image {_} {_} {𝟙} {𝒰₀} (λ ∗ → A)

  ι-BAut : (A : 𝒰₀) → BAut A → 𝒰₀
  ι-BAut A = ι-im₁ (λ ∗ → A)

  ι-BAut-is-injective : ∀ {A : 𝒰₀} → (ι-BAut A) is-injective
  ι-BAut-is-injective {A} = ι-im₁-is-injective (λ ∗₃ → A)

  universal-family-over-BAut′_ :
    (F : 𝒰₀) → (BAut F → 𝒰₀)
  (universal-family-over-BAut′ F) (F′ , p) = F′

  universal-family-over-BAut_ :
    (F : 𝒰₀) → 𝒰₁
  universal-family-over-BAut F = ∑ (universal-family-over-BAut′ F)

  -- the 'unit', i.e. 'refl {e-BAut A}' is the unit of 'Aut A'
  e-BAut : (A : 𝒰₀) → BAut A
  e-BAut A = (A , ∣ (∗ , refl) ∣ )

-- G-sets (Covering Spaces)

  record groups-over-structure-group-of_ {V : 𝒰₀}
    (structure-on-V : homogeneous-structure-on V) : 𝒰₁ where
    field
      BG : 𝒰₀
      Be : BG
      Bφ : BG → BAut (formal-disk-of structure-on-V)
      path-between-units : Bφ(Be) ≈ e-BAut (formal-disk-of structure-on-V)

  module G-structures-on-V-manifolds
    {V′ : 𝒰₀} -- (w : U ─ét→ M) (v : U ─ét→ V′)
    (V : homogeneous-structure-on V′)
    (reduction : groups-over-structure-group-of V)
    (M′ : V -manifold) where
    G-structures : U₁
    G-structures = ∑ (λ (φ : M → BG) → Bφ ∘ φ ⇒ χ)

