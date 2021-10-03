Som=function(L=list(0,1))
{
	res=0
	if (length(L)>0)
	{
		for (i in seq(length(L)))
		{
			res=res+L[[i]]
		}
	}
	res
}

Min=function(L=list(0,1))
{
	res=0
	if (length(L)>0)
	{
		temp=c()
		for (i in seq(length(L)))
		{
			temp=c(temp,L[[i]])
		}
		res=min(temp)
	}
	res
}

Display1=function(L=list(0,1))
{
	res=c()
    for (i in seq(length(L)))
    {
		res=c(res,L[[i]])
    }
	print(res)
}

Display2=function(L=list(list(0,1),2))
{
	res=c()
    for (i in seq(length(L[[1]])))
    {
		res=c(res,L[[1]][[i]])
    }
    res=c(res,L[[2]])
	print(res)
}

Normalize= function(L=list(0,1,0))
	{
		XX=L
		res1=list()
		res2=list()
		if (length(XX)>0)
		{
			s=Min(XX)
			if (s<0)
			{
				for (i in seq(length(XX)))
				{
					XX[[i]]=XX[[i]]-s 
				}
			}
			s=Som(XX)
			if (s>0)
			{
				for (i in seq(length(XX)))
				{
					res1=append(res1,XX[[i]]/s)
					if (i==1)
					{
						res2=append(res2,res1[[length(res1)]])
					}
					else
					{
						res2=append(res2,(res2[[length(res2)]]+res1[[length(res1)]]))
					}
				}
			}
			else
			{
				res1=XX
				res2=XX
			}
		}
		res=list(res1,res2)
		res
	}
    
Choice=function(L=list(0.5,0.25,0.25))
	{
		XX=L
		if (length(XX)>0)
		{
			if (Som(XX)==0)
			{
				for (i in seq(length(XX)))
				{
					XX[[i]]=1
				}
			}
			XX=Normalize(XX)[[2]]
			a=runif(1,0,1)
			res=1
			while ((res<length(XX))&&(a>XX[[res]]))
			{
				res=res+1
			}
		}
		else
		{
			res=list()
		}
		res
	}

Generate=function(M=list(list(0,1/3,1/3,1/3),list(0,0,1/2,1/2),list(0,1/2,0,1/2),list(0,1/2,1/2,0)))
	{
		NN=M
		p=1
		n=1
		while (n<length(NN))
		{
			pnew=Choice(NN[[p]])
			for (i in seq(length(NN)))
			{
				if (i!=p)
				{	
					NN[[i]][[pnew]]=0
				}
			}
			for (i in seq(length(NN)))
			{
				if (i!=pnew)
				{
					NN[[p]][[i]]=0
					NN[[p]]=Normalize(NN[[p]])[[1]]
				}
			}
			p=pnew
			n=n+1		
		}
		NN
	}

SolCost=function(N=list(list(0,1/2,1/2),list(0,0,1),list(0,1,0)),C=list(list(10,15),list(15,20)))
{
    XX=list()
    Cost=0
    n=1
    p=1
    while (n<(length(N)))
	{
		i=2
		while ((i<=length(N))&&(N[[p]][[i]]==0))
		{
			i=i+1
		}	
		if (i<=length(N))
		{
			XX=append(XX,list(i-1))
			p=i
			Cost=Cost+C[[n]][[p-1]]
		}
        n=n+1
	}
	res=list(XX,Cost)
	res
}

Update=function(Mold,Xold,Cold,N,Cnew)
{
	MM=Mold
	temp=SolCost(N,Cnew)
	XX=temp[[1]]
	cc=temp[[2]]
	if (cc<Cold)
	{
		#print("Here")
		for (i in seq(length(N)))
		{
			for (j in seq(length(N)))
			{
				if (N[[i]][[j]]>0)
				{
					MM[[i]][[j]]=MM[[i]][[j]]+N[[i]][[j]]
				}
			}
			MM[[i]]=Normalize(MM[[i]])[[1]]
		}
		res=list(MM,XX,cc)
	}
    else
	{
		#print("Out")
		res=list(Mold,Xold,Cold)
	}
	res
}


Nmax=100       
C=list(list(10,15,11),list(15,20,12),list(15,20,18))
M=list(list(0,1/3,1/3,1/3),list(0,0,1/2,1/2),list(0,1/2,0,1/2),list(0,1/2,1/2,0))
N=Generate(M)

temp=SolCost(N,C)
X=temp[[1]]
c=temp[[2]]
Display2(temp)
for (i in seq(Nmax))
{
	N=Generate(M)
	Display2(SolCost(N,C))
	temp=Update(M,X,c,N,C) 
	M=temp[[1]]
	X=temp[[2]]
	c=temp[[3]]
}
    
print("La solution est X= ")
Display1(X)
print("Le cout optimal est c= ")
print(c)