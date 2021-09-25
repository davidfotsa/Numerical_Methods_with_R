f = function(x)
	{ 
		(x^4) -(4*(x^3)) - (8*(x^2)) + 48*x + 3
	}
x = 3
s = x
T = 10
e = 1
k = 0.5
while (T>=e)
	{
		d = rnorm(1,mean = 0, sd =T) 
		if (f(x+d) < f(x))
		{	 
			x = x+d 
			s = x	
		}
		else
		{
			D = f(x+d)-f(s)
			a = runif(1, min = 0, max = 1)
			if (exp(-D/T)>=a)
			{	
				x = x+d
		 		T = k*T
	 		}
		}
	}
print("Xopt = ")
print(s)