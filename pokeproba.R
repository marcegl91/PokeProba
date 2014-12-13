source("functions.R")
writeLines("Welcome to the Pokemon Catch Rate Probability System!")
writeLines("This system computes the probability to catch a Pokemon in the Pokemon franchise games.")
writeLines("In order to calculate this probability, is used a maximum likelihood estimator for a random sample of n Bernoulli trials.")
writeLines("This estimator is the relative frequency of successes in the n Bernoulli trials. Have fun!")

checkData()

pokeHP <- read.csv("data/pokemon_stats.csv")
# stat_id == 1 is HP, so only select that rows, and columns "id" and "base_stat"
pokeHP <- pokeHP[pokeHP$stat_id == 1, c(1,3)]
# change column name
colnames(pokeHP)[2] <- "max_hp"

pokeCatchRate <- read.csv("data/pokemon_species.csv", stringsAsFactors = FALSE)
# only select columns "id", "identifier", "generation_id", "capture_rate" 
pokeCatchRate <- pokeCatchRate[, c(1,2,3,10)]

# merge all the data
pokeList <- merge(x = pokeHP, y = pokeCatchRate, by.x = "pokemon_id", by.y = "id")

rm("pokeHP", "pokeCatchRate")

menu()
rm(list = ls())