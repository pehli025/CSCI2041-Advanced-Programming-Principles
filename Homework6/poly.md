### I. ``deg (polycomp p1 p2)`` ≡ ``(deg p1)*(deg p2)``
### Property
P(p1): ∀ p2: polyExpr deg(polycomp p1 p2) ≡ (deg p1) * (deg p2)

### Base case
P(Int x): ∀ p2: polyExpr deg(polycomp (Int x) p2) ≡ (deg(Int x))*(deg p2)
 ≡ deg(Int x) ≡ (deg (Int x))*(deg p2) [eval polycomp]
 ≡ 0 ≡ 0*(deg p2) [eval deg]
 0 ≡ 0

P(X): ∀p2: polyExpr deg(polycomp X p2) ≡ (deg X)*(deg p2)
 ≡ deg p2 ≡ (deg X)*(deg p2) [eval polycomp]
 ≡ deg p2 ≡ 1*(deg p2) [eval deg]
 ≡ deg p2 ≡ deg p2

### Inductive case
We have two cases one for mul and add

P(e1)&&P(e2) => P(Mul(e1,e2)) ≡  deg(polycomp e1 p2) ≡ (deg e1)*(deg p2)&&deg(polycomp e2 p2) ≡ (deg e2)*(deg p2) ≡ deg (polycomp Add(e1,e2) p2) ≡ deg(Add(e1,e2))*(deg p2)

P(p1)&&P(p2) => P(Add(p1,p2)) ≡ deg(polycomp  e1 p2) ≡ (deg e1)*(deg p2)&&deg(polycomp e2 p2) ≡ (deg e2)*(deg p2) ≡ deg(polycomp Mul(e1,e2), p2) ≡ (deg (Mul(e1,e2)))*(deg) p2

### Inductive Hypothesis
deg(polycomp e1 p2) ≡ (deg e1)*(deg p2) && deg(polycomp e2 p2) ≡ (deg e2)*(deg p2)

Mul case:
RS:
  deg(Mul(e1,e2)*(deg p2) ≡ [(deg e1) + (deg e2)]*(deg p2) [eval deg]
  ≡ [(deg e1)+(deg e2)]+(deg e2)*(deg p2) [dist prop]
  ≡ ((deg e1)*(deg p2))+((deg e2)*(deg p2)) ≡ ((deg e1)*(deg p2))+((deg e2)*(deg p2))
LS:
  deg(polycomp Mul(e1, e2) p2) ≡ deg(Mul(polycomp e1 p2, polycomp e2 p2)) [eval polycomp]
  ≡ deg(polycomp e1 p2) + deg(polycomp e2 p2) [eval deg]
  ≡ (deg e1)*(deg p2) + (deg e2)*(deg p2) [IH]

Add:
RS:
  ≡ deg (Add(e1,e2))*(deg p2)
  ≡ max(deg e1)(deg d2)*(deg p2) [eval deg]
  ≡ max
LS:
  deg(polycomp Add(e1,e2)p2) ≡ deg(Add(polycomp e1 p2, polycomp e2 p2)) [eval polycomp]
  ≡ max(deg(polycomp e1 p2))(deg(polycomp e2 p2)) [eval deg]
  ≡ max (deg e1)*(deg p2)(deg e2)*(deg p2) [IH]
