# Naturals

### ``pow_nat``
### Property
∀ n: nat, P(n) ∀x: float pow x (to_int n) ≡ pow_nat x n

### Base case
P(Zero): pow x 0 ≡ pow_nat x Zero
≡ 1 ≡ 1
### Inductive case
∀ n: nat P(n) ≡ P(Succ n) => pow x (to_int(Succ n)) ≡ pow_nat x (Succ n)

###Inductive Hypothesis
pow x (to_int n) ≡ pow_nat x n ≡ pow_nat x (Succ n)
≡ x *. pow_nat x n [eval pow_nat]
≡ x *. pow x (to_int n) [eval IH]
≡ pow x (to int(Succ n)) [reverse pow_nat]

### ``geq_nat``
### Property
P(n): ∀ m: nat geq_nat m n ≡ to_int m >= to_int n

### Base Case:
P(Zero): ∀ m: nat geq_nat m Zero ≡ to_int m >= to_int Zero
≡ geq_nat m zero ≡ to_int m >= 0
≡ true ≡ true

### Inductive case
∀ n: nat P(n) => P(Succ n) => geq_nat m (Succ n) ≡ to_int m >= to_int (Succ n)

###Inductive Hypothesis
geq_nat m n ≡ (to_int m) >= (to_int n)

we have two Hypothesis the if statement and the else
Lets start with the if m = n then
≡ false ≡ (to_int m) >= (to_int (Succ n))
≡ false ≡ (to_int m) >= 1+ (to_int n)
≡ false ≡ false [m = n, 1 + n would indicate that m < n+1]

now for the else statement
≡ geq_nat m n ≡ (to_int m) >= (to_int(Succ n))
≡ (to_int m) > (to_int(Succ n)) [Not equal]
≡ (to_int m) > 1 + (to_int 
