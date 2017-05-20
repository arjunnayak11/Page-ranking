#We can represent a graph of connected nodes as a matrix

#Suppose we have a graph with the following edges:
#1->2, 2->4, 4->2, 2->3, 1->4, 3->1

#Step 1: Represent this graph as a matrix
x<-matrix(0,4,4)
x[1,2]=1
x[2,4]=1
x[4,2]=1
x[2,3]=1
x[1,4]=1
x[3,1]=1
x<-t(x)
#Step 2: Normalize x and multiply by d
x=t(t(x)/colSums(x))
x=x*.85
x[1,1]=-1
x[2,2]=-1
x[3,3]=-1
x[4,4]=-1

#Step 3: Represent LHS as the (1-d)/N
y <- matrix(data=c(-.15/4,-.15/4,-.15/4,-.15/4), nrow=4, ncol=1, byrow=FALSE)


#Step 4: Solve the linear system of equations
result<-solve(x,y)

#Step 5: Normalize to be a probablity (sum of PageRanks = 1.0)
result<-result/sum(result)
result



####Using the igraph package
library(igraph)
x<-matrix(0,4,4)
x[1,2]=1
x[2,4]=1
x[4,2]=1
x[2,3]=1
x[1,4]=1
x[3,1]=1
ga<-graph.adjacency(x)
plot(ga)
page.rank(ga)





###Example of iterative convergence

newa=1/4
newb=1/4
newc=1/4
newd=1/4



for(i in 1:100){
  
  
  a=.15/4+.85*(newc)
  newa=a
  print(paste("a: ",newa))
  b=.15/4+.85*(newa/2+newd)
  newb=b
  print(paste("b: ",newb))
  c=.15/4+.85*(newb/2)
  newc=c
  print(paste("c: ",newc))
  d=.15/4+.85*(newb/2+newa/2)
  newd=d
  print(paste("d: ",newd))
  
  
}
