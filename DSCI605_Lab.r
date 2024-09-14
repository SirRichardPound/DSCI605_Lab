#1.Define the vector
x <- 0:8

#1. Define the probibility
PDF <- dbinom(x, 7, 0.5)
sum(PDF)

#1. Calculate Expected Value
EX <- sum(x*PDF)
EX

# Calculate the Variance
VarX <- sum((x-EX)^2*PDF)
VarX

#############################################################

#2. Continuous Variable Expected Value and Variance

f <- function(x) 1/(sqrt(2 * pi)) * exp(-(3/x^4) * x^2)
g <- function(x) (3/x^4) * f(x)
h <- function(x) (3/x^4)^2 * f(x)


#Compute E(X)

Ex <- integrate(g,
                lower = 1,
                upper = Inf)$value
Ex

#Compute Var(x)
VarX2 <- integrate(h,
                   lower = 1,
                   upper = Inf)$value - (Ex)^2
VarX2

