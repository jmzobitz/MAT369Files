
nSteps = 100

x = array(0,dim=nSteps)

for (i in 2:nSteps) {
  if (runif(1)<0.5) {x[i]=x[i-1]+1}
  else x[i]=x[i-1]-1
}

print(x)
plot(x,type='l')

print(mean(x))
print(sd(x))

