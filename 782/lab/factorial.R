# An R Factorial Function
### This function uses recursion to compute factorials.
fact = function(n)
	if (n <= 0) 1 else n * fact(n - 1)

