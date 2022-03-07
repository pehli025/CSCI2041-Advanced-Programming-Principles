### I. ``map id l ≡ l``
Asked to prove that ∀ n : nat, ∀ 𝓍 : float: geq_nat m n ≡ (to_int m) >= (to_int n)
So we define ∀ l: map id l ≡ l
### Base Case:
P([]): map id [] ≡ [] => [] ≡ []

### Inductive case:
∀ l: P(l) ≡ map id l => mapd id (h::l) ≡ h::l

## Inductive Hypothesis
map id l ≡ l => (id::h) :: (map f t) [eval of map]
≡ h::(map id l) [eval of id]
≡ h::l [IH]

### II. ``length l ≡ length (reverse l)``
### Property
P(l): length l ≡ length(reverse l)

### Base Case
P([]): length [] ≡ length(reverse [])
≡ lenth [] ≡ length [] [eval reverse]
0 ≡ 0 [eval length]

### Inductive Case
∀ l: int list, ∀ h: P(l): length h::l ≡ length(reverse (h::l))

### Inductive Hypothesis
length l ≡ length(reverse l) ≡ length h::l ≡ length(reverse (h::l))
≡ length(h::l) ≡ length(tail_rev (h::t) [])
≡ length(tail_rev (h::[]))
≡ length l ≡ length(tail_rev l (h::[])) [Lemma]
≡ 1 + length l
≡ length (h::l)

### Lemma
### Property
∀ l: int list P(l) ∀ l2: int list length l1 + length l2 ≡ length(tail_rev l1 l)

### base case
P([]) lenth [] + length l2 ≡ 0 + length l2 ≡ length (tail_rev [] l2)
≡ length l2 ≡ length l2

###inductive Case
∀ l: int list P(l) => P(h::l) => length h::l1 + l2 ≡ length(tail_rev (h::l1) l2)

###Inductive Hypothesis
length l1 + length l2 ≡ length(tail_rev l1 l2) ≡ length (h::l1) + length l2 ≡ length(tail_rev (h::l1) l2)
≡ length(tail_rev l1 (h::l2))[eval tail_rev]
≡ length l1 + length (h::l2))[eval IH]
≡ 1 + length l1 + length l2[eval length]
≡ length (h::l1) + length l2[reverse length]
