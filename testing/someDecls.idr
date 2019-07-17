module testing.someDecls

n : Nat
n = S (S Z)

m : Nat
m = n + n

f : Nat -> Nat
f k = S k
