# LDI 2019-2020: Agda project

# Common definitions

```agda
module submission where

infixr 2 _∧_
data _∧_ (A : Set) (B : Set) : Set where
  ⟨_,_⟩ : A → B → A ∧ B

fst : {A B : Set} → A ∧ B → A
fst ⟨ a , _ ⟩ = a

snd : {A B : Set} → A ∧ B → B
snd ⟨ _ , b ⟩ = b

data ⊤ : Set where
  tt : ⊤

A→⊤ : {A : Set} → A → ⊤
A→⊤ _ = tt

data ⊥ : Set where

⊥-elim : {A : Set} → ⊥ → A
⊥-elim ()

infix 3 ¬_
¬_ : Set → Set
¬ A = A → ⊥

infixr 1 _∨_
data _∨_ (A : Set) (B : Set) : Set where
    left : A → A ∨ B
    right : B → A ∨ B

case : {A B C : Set} → (A → C) → (B → C) → A ∨ B → C
case f g (left x) = f x
case f g (right x) = g x

Π : (A : Set) → (B : A → Set) → Set
Π A B = (a : A) → B a

forAll : {A : Set} → (B : A → Set) → Set
forAll {A} B = Π A B

∀-syntax = forAll
infix 0 ∀-syntax
syntax ∀-syntax (λ a → B) = ∀[ a ] B

apply : {A : Set} → {B : A → Set} → Π A B → (a : A) → B a
apply f x = f x

data Σ (A : Set) (B : A → Set) : Set where
    ⟨_,_⟩ : (a : A) → B a → Σ A B

thereExists : ∀ {A : Set} (B : A → Set) → Set
thereExists {A} B = Σ A B

∃-syntax = thereExists
infix 0 ∃-syntax
syntax ∃-syntax (λ x → B) = ∃[ x ] B

∃-elim : {A : Set} {B : A → Set} {C : Set} → (∀ (a : A) → B a → C) → Σ A B → C
∃-elim a→b→c ⟨ a , b ⟩ = a→b→c a b

dfst : {A : Set} {B : A → Set} → Σ A B → A
dfst ⟨ a , _ ⟩ = a

dsnd : {A : Set} {B : A → Set} → (p : Σ A B) → B (dfst p)
dsnd ⟨ _ , b ⟩ = b
```

# `where` definitions

From time to time it may be convenient to use Haskell-style `where` definitions.
This example shows how to use them:

```agda
where-example : {A B : Set} → (A → B → A) → B → A → A
where-example {A} {B} f b a = g b (g b a)
  where
  g : B → A → A
  g b' a' = f a' b'
```

Incidentally, the example above also shows how to access implicit parameters such as `{A}` and `{B}`.

# Propositional logic

```agda
-- BEGIN PROBLEM p01
p01 : {P Q R : Set} → (P → Q) → (Q → R) → P → R
p01 {P} {Q} {R} p_q q_r p = (q_r (p_q p))
-- END PROBLEM
```

```agda
-- BEGIN PROBLEM p02
p02 : {P Q R S : Set} → (P → Q) ∧ (R → S) → P ∧ R → Q ∧ S
p02 {P} {Q} {R} {S} and_impl and = ⟨ ((fst and_impl) (fst and)) , ((snd and_impl) (snd and)) ⟩
-- END PROBLEM
```

```agda
-- BEGIN PROBLEM p03
p03 : {P : Set} → ¬ (P ∧ ¬ P)
p03 {P} ⟨ p , n_p ⟩ = n_p p
-- END PROBLEM
```

```agda
-- BEGIN PROBLEM p04
p04 : {P Q : Set} → (P ∨ Q) ∧ ¬ P → Q
p04 {P} {Q} ⟨ or_p_q , n_p ⟩ = case (λ p → ⊥-elim ( n_p p )) (λ q → q) or_p_q
-- END PROBLEM
```

```agda
-- BEGIN PROBLEM p05
p05 : {P Q : Set} → (P → Q) → ¬ ¬ (¬ P ∨ Q)
p05 {P} {Q} p_q impos = (λ n_p → impos (left n_p)) (λ p → impos (right (p_q p)))
-- END PROBLEM
```

# First-order logic

```agda
module IFOL
    (Eq : {A : Set} → A → A → Set)
    (subst : {A B : Set} → (f : A → B) → ∀[ a1 ] ∀[ a2 ] (Eq a1 a2 → Eq (f a1) (f a2)))
    (trans : {A : Set} → (a1 a2 a3 : A) → Eq a1 a2 → Eq a2 a3 → Eq a1 a3)
  where
-- In the statement of the exercises below we use the module parameter `Eq` to represent the type of the equality relation.
-- The meaning of `Eq` is axiomatised by using the other two parameters `subst` (expressing substitutivity) and `trans` (transitivity).
-- Thus, in the solutions one can (and in fact has to) use `subst` and `trans`.

-- IMPORTANT: every line of code below must be indented with 2 spaces
-- (we are inside the module IFOL, otherwise we cannot see the parameters)

  inj : {A B : Set} → (A → B) → Set
  inj {A} {B} f = ∀[ a1 ] ∀[ a2 ] (Eq (f a1) (f a2) → Eq a1 a2)

  surj : {A B : Set} → (A → B) → Set
  surj {A} {B} f = ∀[ b ] ∃[ a ] Eq (f a) b

  -- this is input by writing "\o"
  infix 20 _∘_
  _∘_ : {A B C : Set} → (A → B) → (B → C) → A → C
  (f ∘ g) a = g (f a)
```

```agda
-- BEGIN PROBLEM p06
  p06 : {A B : Set} {f : A → B} → inj f → ∀[ x ] ∀[ y ] (¬ Eq x y → ¬ Eq (f x) (f y))
  p06 {A} {B} injective x y n_eq f_eq = n_eq (injective x y f_eq)
-- END PROBLEM
```

```agda
-- BEGIN PROBLEM p07
  p07 : {A B C : Set} → (f : A → B) → (g : B → C) → inj (f ∘ g) → inj f
  p07 {A} {B} {C} f g inj_com a1 a2 eq_f = inj_com a1 a2 (subst g (f a1) (f a2) eq_f)
-- END PROBLEM
```

```agda
-- BEGIN PROBLEM p08
  p08 : {A B C : Set} → (f : A → B) → (g : B → C) → inj f → inj g → inj (f ∘ g)
  p08 {A} {B} {C} f g inj_f inj_g a1 a2 eq_g = inj_f a1 a2 (inj_g (f a1) (f a2) eq_g)
-- END PROBLEM
```

```agda
-- BEGIN PROBLEM p09
  p09 : {A B C : Set} → (f : A → B) → (g : B → C) → surj (f ∘ g) → surj g
  p09 f g surj_com c = ⟨ f (dfst (surj_com c)) , dsnd (surj_com c) ⟩
-- END PROBLEM
```

```agda
-- BEGIN PROBLEM p10
  -- rozpakować obie wartości naraz ze surfj i surjg
  p10 : (A B C : Set) (f : A → B) (g : B → C) → surj f → surj g → surj (f ∘ g)
  p10 A B C f g surjf surjg c = let b = dfst (surjg c)
                                    a = dfst (surjf b)
                                    f_a=b = dsnd (surjf b)
                                    g_b=c = dsnd (surjg c)
                                    g_f_a=g_b = subst g (f a) b f_a=b
                                    g_f_a=c = trans (g (f a)) (g b) c g_f_a=g_b g_b=c in ⟨ a , g_f_a=c ⟩
-- END PROBLEM
```
