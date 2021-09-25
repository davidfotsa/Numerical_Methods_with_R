f = function(x)
{ 
	(x^4) -(4*(x^3)) - (8*(x^2)) + 48*x + 3
}

n = 1000
X = rnorm(n,mean = 0, sd = 10000) 
xopt = X[1]

for (k in seq(1,n,1))
{
	if (f(X[k]) < f(xopt))
	{
		xopt = X[k]
	}
}
	
r = 10^(-1)
imax = 0

i = 0
tmp=c()
for (k in seq(n))
{
	tmp=c(tmp,abs(X[k] - xopt))
}

while((i<imax) && (max(tmp)>= r ))
{
	for (k in seq(1,n,1))
	{
		a = runif(1, min = 0, max = 1)
		X[k] = X[k] + a*(xopt - X[k])
	}
	
	for (k in seq(1,n,1))
	{
		if (f(X[k]) < f(xopt))
		{
			xopt = X[k]
		}
	}
	
	tmp=c()
	for (j in seq(n))
	{
		tmp=c(tmp,abs(X[j] - xopt))
	}
	i = i+1
}
print("L'optimum est x = ")
print(xopt)
			

