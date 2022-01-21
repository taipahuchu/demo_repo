rpareto = function(n, alpha, lambda) {
  rp = lambda * ((1 - runif(n))^(-1/alpha) - 1)
  rp}

#part (i)
set.seed (123)

A_vec = rpareto(n = 25000, alpha = 3, lambda = 1)

head(A_vec, 8)

#part (ii)
A_exceed_u = function(A, u){
  E = pmax(A - u, 0) 
  output = E[E!=0]
  output}

head(A_exceed_u(A = A_vec, u = 1), 8)


#part(iii)
F_u = function(A_greater_than_u) {
  y = vector(length = 101)
  for (i in 1:101) {
    y[i] = length(A_greater_than_u[A_greater_than_u <= 0.1 * (i-1)]) / length(A_greater_than_u) 
  }
  y}

head(F_u(A_exceed_u(A = A_vec, u = 1)), 8)


#part(iv)
x = seq(from = 0, to = 10, by = 0.1)

plot(x, F_u(A_greater_than_u = A_exceed_u(A = A_vec, u = 1)), type = "l", main = "Values of F_u against x for u = 1, 2, 3 and 4", col = "red", ylab = "F_u")

lines(x, F_u(A_greater_than_u = A_exceed_u(A = A_vec, u = 2)), col = "yellow")
lines(x, F_u(A_greater_than_u = A_exceed_u(A = A_vec, u = 3)), col = "blue")
lines(x, F_u(A_greater_than_u = A_exceed_u(A = A_vec, u = 4)), col = "green")

legend("bottomright", legend = c("u = 1", "u = 2", "u = 3", "u = 4"), col = c("red", "yellow", "blue", "green"), pch = 7)
