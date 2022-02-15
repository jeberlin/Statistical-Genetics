#How many generations would it take to reach Hardy-Weinberg Equilibrium?
  #And what are the genotype and allele frequencies in equilibrium?

#Goal: second generation


#Create an object that represents the population, with the following attributes:
  #Total population size of 100
  #10% are AA
  #90% are aa


# single AA individual
AA <- c("A","A")
# single aa individual
aa <- c("a","a")
# 10 AA individuals
all.AA <- matrix(rep(AA,10),nrow=10)
# 90 aa individuals
all.aa <- matrix(rep(aa,90), nrow = 90)
# total starting population
population <- rbind(all.AA, all.aa) #Holds 10% AA and 90% aa. It is the 'population' we'll sample from.



#Select parents at random to mate
#From the randomly selected parents, choose alleles at random to pass to
#offspring (maintain constant population size of 100)

# Create empty matrix to fill in new generation
gen1 <- matrix(NA,nrow=100,ncol=2) #This is the first generation of offspring 


for(i in 1:100){
  parents <- sample(100, size=2) #Random sample of which two beings will be the parents
  allele1 <- sample(population[parents[1], ],size=1) #The first parent has one allele randomly chosen
  allele2 <- sample(population[parents[2], ],size=1) #The second parent has one allele randomly chosen
  gen1[i,] <- c(allele1, allele2)
}

#Question: what is i? What does it represent in terms of the population,genotypes, or otherwise?
    # i is the iteration number. Here, it is the offspring identifier.


#How can we write code to find the relative frequences of the AA, Aa and aa genotypes in it? 
#How about the A and a alleles?

apply(gen1, 1, identical, c("a","a"))
#What does this do? What do you need to change/add in order to make it do everything you need for the present task?
  #This results in True/False for if the observation has a,a or not.  
  #We need to get it to give counts and also read Aa the same as aA

#This is how many aa
sum(apply(gen1, 1, identical, c("a","a"))) 
#This is how many AA 
sum(apply(gen1, 1, identical, c("A","A"))) 
#This is how many Aa (includes Aa and aA)
sum(apply(gen1, 1, identical, c("A","a")),
    apply(gen1, 1, identical, c("a","A")))


#################################################
#Now use gen1 as the parents, and simulate the next generation.
# Create empty matrix to fill in new generation
gen2 <- matrix(NA,nrow=100,ncol=2) #This is the first generation of offspring 


for(i in 1:100){
  parents <- sample(100, size=2) #Random sample of which two beings will be the parents
  allele1 <- sample(gen1[parents[1], ],size=1) #The first parent has one allele randomly chosen
  allele2 <- sample(gen1[parents[2], ],size=1) #The second parent has one allele randomly chosen
  gen2[i,] <- c(allele1, allele2)
}


#This is how many aa
sum(apply(gen2, 1, identical, c("a","a"))) 
#This is how many AA 
sum(apply(gen2, 1, identical, c("A","A"))) 
#This is how many Aa (includes Aa and aA)
sum(apply(gen2, 1, identical, c("A","a")),
    apply(gen2, 1, identical, c("a","A")))

## Phenotype frequencies ##
#This is how many recessive
sum(apply(gen2, 1, identical, c("a","a"))) 
#This is how many dominant
sum(apply(gen2, 1, identical, c("A","A")),
    apply(gen2, 1, identical, c("A","a")),
    apply(gen2, 1, identical, c("a","A")))
