install.packages("markovchain") # if not installed
library(markovchain)
ggg
#part (i)
markovchain = function(p,q,r){
  P = matrix(data = c(p,1-p,0,q,0,1-q,0,r,1-r), nrow=3, byrow=TRUE)
  mc = new("markovchain", transitionMatrix = P)
  mc}

#part(ii)
statdist = matrix(data = 0, nrow = 3, ncol = 9)

for(i in 1:9){
  q = 0.1*i
  mc = markovchain(p = 0.75, q, r = 0.25)
  statdist[,i] = steadyStates(mc)}

statdist

#part(iii)
q = seq(from = 0.1, to = 0.9, by = 0.1)

plot(q, statdist[1,], type = "l", main = "Stationary Distributions of X_t as a function of q", ylab = "Probability", col = "red")

lines(q, statdist[2,], col = "black")
lines(q, statdist[3,], col = "blue")

legend("top", legend = c("State 1", "State 2", "State 3"), col = c("red", "black", "blue"), pch = 7)














