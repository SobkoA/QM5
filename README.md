# HM.5


#3
sum(dbinom(0:2, 9, .5))
barplot(dbinom(0:9, 9, .5))
#0.08984375
sum(dbinom(0:4, 18, .5))
barplot(dbinom(0:18,18,.5))

#4
#F:M; 1:1.2  
sum(dbinom(3:6, 6, 1/2.2))
#0.4323509

#5a
probs = c(0.2,0.3,0.1,0.15,0.25)

strategy1 = c(5,11,13,15.5,16)
strategy2 = c(-4,15,17.5,20.8,20)

E = function(s) sum(s*probs)
V = function(s) sum(probs*(s-E(s))^2)

E(strategy1)
E(strategy2)
V(strategy1)
V(strategy2)

util =function(s) E(s)-sqrt(V(s))

u1 = util(strategy1)
print(u1)
u2 = util(strategy2)
print(u2)

(u1>u2)
#the company should implement Str1 

#b
howoften <- 10000
results <- rep(NA, howoften)
for (i in seq_len(howoften)) {
  results[i] <- sum(sample(strategy1, 1, replace = TRUE))
}
mean(results)
var(results)

mean(sample(strategy1, size=100000 ,prob = probs, replace = TRUE))

howoften <- 10000
results <- rep(NA, howoften)
for (i in seq_len(howoften)) {
  results[i] <- sum(sample(strategy2, 1, replace = TRUE))
}
mean(results)
var(results)

mean(sample(strategy2, size=100000 ,prob = probs, replace = TRUE))

#6a

totFr = 50000
#probabilty of A
PA = 0/totFr


PB = 1000/totFr
print(PB)
#PB = 0.02

PC = (50000-11311)/totFr
print(PC)

PD = (1+10)/totFr
print(PD)
#PD = 0.00022

#B

PAuB = PB
PAiB = 0
PBuC = PB + PC
PBiC = 0 
PCuD = PD + PC

'x= function(type){
  switch (as.character(type),
          "2"= {x = 2-2},
          "4" = {x = 4-2},
          "32" = {x = 32-2},
          "900" = {x =900-2},
          "10000" = {x= 10000- 2},
          {-2})
}'

x= c(-2,0,2,30,898,9998)
pro= c(((50000-11311)/50000), (1/5),(1/50), (300/50000),(10/50000),(1/50000))

Ex = function(x){
  sum(x*pro)
}
Ex(x)
VR = function(x){
  sum(pro*(x-Ex(x))^2)
}
VR(x)
#7a

sum(dbinom(11:20, 20, .2))
#0.9998983


