checkData <- function() {
  if(!file.exists("data")) {
    writeLines("Creating data directory...")
    dir.create(file.path(getwd(),"data"))
  }
  
  if(!file.exists("data/pokemon_stats.csv")){
    writeLines("Downloading necessary files...")
    fileUrl <- "https://raw.githubusercontent.com/veekun/pokedex/master/pokedex/data/csv/pokemon_stats.csv"
    download.file(fileUrl, destfile ="data/pokemon_stats.csv", method = "curl")
  }
  
  if(!file.exists("data/pokemon_species.csv")){
    writeLines("Downloading necessary files...")
    fileUrl <- "https://raw.githubusercontent.com/veekun/pokedex/master/pokedex/data/csv/pokemon_species.csv"
    download.file(fileUrl, destfile ="data/pokemon_species.csv", method = "curl")
  }
}

generation_1 <- function(id, current_hp, pokeball, pokestatus) {
  
  if(pokeball == 4) return (1)
  
  if(pokeball == 1) max <- 255
  else if(pokeball == 2) max <- 200
  else max <- 150
  
  r_number <- runif(1, 0, max)
  
  if((pokestatus == 3 | pokestatus == 6) & r_number < 25) return(1)
  
  if((pokestatus == 4 | pokestatus == 2 | pokestatus == 5) & r_number < 12) return(1)
  
  capture_rate <- pokeList[pokeList$pokemon_id == id, "capture_rate"]
  if((r_number - current_hp) > capture_rate) return(0)
  
  r_number <- runif(1, 0, 255)
  
  max_hp <- pokeList[pokeList$pokemon_id == id, "max_hp"]
  
  if(pokeball == 2) ball <- 8
  else ball <- 12
  f <- (max_hp * 255 * 4) / (current_hp * ball)
  
  if(f >= r_number) return(1)
  else return(0)
}

generation_2 <- function(id, current_hp, pokeball, pokestatus) {
  max_hp <- pokeList[pokeList$pokemon_id == id, "max_hp"]
  capture_rate <- pokeList[pokeList$pokemon_id == id, "capture_rate"]
  
  if(pokeball == 4) return(1)
  else if(pokeball == 3) ball <- 2
  else if(pokeball == 2) ball <- 1.5
  else if(pokeball == 1) ball <- 1
  
  if(pokestatus == 3 | pokestatus == 6) status <- 10
  else if(pokestatus == 2 | pokestatus == 4 | pokestatus == 5) status <- 5
  else status <- 0
  
  if(3 * max_hp > 255) {
    first_number <- floor((3 * max_hp ) / 2)
    first_number <- floor(first_number / 2)
    
    second_number <- floor(( 2 * current_hp) / 2)
    second_number <- floor(second_number / 2)
    
    if(second_number == 0) second_number <- 1
    
    a <- floor((first_number - second_number) * (capture_rate * ball) / (3 * max_hp) + status)
  } else a <- floor((3 * max_hp - 2 * current_hp) * (capture_rate * ball) / (3 * max_hp) + status)
  
  r_number <- runif(1, 0, 255)
  
  if (r_number <= a) return(1)
  else return(0)
}

generation_3_4 <- function(id, current_hp, pokeball, pokestatus) {
  max_hp <- pokeList[pokeList$pokemon_id == id, "max_hp"]
  capture_rate <- pokeList[pokeList$pokemon_id == id, "capture_rate"]
  
  if(pokeball == 4) ball <- 255
  else if(pokeball == 3) ball <- 2
  else if(pokeball == 2) ball <- 1.5
  else if(pokeball == 1) ball <- 1
  
  if(pokestatus == 3 | pokestatus == 6) status <- 2
  else if(pokestatus == 2 | pokestatus == 4 | pokestatus == 5) status <- 1.5
  else status <- 1
  
  a <- ((3 * max_hp - 2 * current_hp) * (capture_rate * ball) / (3 * max_hp)) * status
  
  b <- ((2 ^(16)) - 1)* ((a/((2^(8))-1)) ^(1/4))
  
  result <- replicate(4, b >= runif(1, 0, 65535))
  
  if (sum(result) == 4) return(1)
  else return(0)
}

calculate <- function(generation_X, id, current_hp, pokeBall, pokeStatus, times) {
  return (mean(replicate(times,generation_X(id, current_hp, pokeBall, pokeStatus)))*100)
}

pokeproba <- function(id) {
  current_hp <- as.numeric(readline(paste("Enter current HP (it has to be greater than 0 and less than ", pokeList[id, "max_hp"], "): ", sep = "")))
  
  if (is.na(current_hp) | current_hp < 1 | current_hp > pokeList[pokeList$pokemon_id == id, "max_hp"]) {
    writeLines("Try again")
    return()
  }
  
  writeLines("Type of Pokeballs:")
  writeLines("1 - Poke Ball ; 2 - Great Ball; 3 - Ultra Ball ; 4 - Master Ball")
  pokeBall <- as.numeric(readline("Choose a Pokeball: "))
  
  if (is.na(pokeBall) | pokeBall < 1 | pokeBall > 4) {
    writeLines("Try again")
    return()
  }
  
  writeLines("Type of Pokemon status:")
  writeLines("1 - Normal ; 2 - Burned ; 3 - Frozen ; 4 - Paralyzed ; 5 - Poisoned ; 6 - Asleep")
  pokeStatus <- as.numeric(readline("Choose a status: "))
  
  if (is.na(pokeStatus) | pokeStatus < 1 | pokeStatus > 6) {
    writeLines("Try again")
    return()
  }
  
  times <- as.numeric(readline("Choose the quantity of tests: "))
  
  if (is.na(times) | times < 1 ) {
    writeLines("Try again")
    return()
  }
  
  cat("Probability to catch in Generation I: ")
  
  if (pokeList[pokeList$pokemon_id == id, "generation_id"] <= 1 ) {
    cat(calculate(generation_1, id, current_hp, pokeBall, pokeStatus, times),"%")
  } else cat ("It can't be calculated because this pokemon doesn't exist in this generation")
  
  cat("\nProbability to catch in Generation II: ")
  if (pokeList[pokeList$pokemon_id == id, "generation_id"] <= 2 ) {
    cat(calculate(generation_2, id, current_hp, pokeBall, pokeStatus, times),"%")
  } else cat ("It can't be calculated because this pokemon doesn't exist in this generation")
  
  cat("\nProbability to catch in Generation III: ")
  result <- calculate(generation_3_4, id, current_hp, pokeBall, pokeStatus, times)
  
  if (pokeList[pokeList$pokemon_id == id, "generation_id"] <= 3 ) {
    cat(result,"%")
  } else cat ("It can't be calculated because this pokemon doesn't exist in this generation")
  
  cat("\nProbability to catch in Generation IV: ")
  if (pokeList[pokeList$pokemon_id == id, "generation_id"] <= 4 ) {
    cat(result,"%")
  } else cat ("It can't be calculated because this pokemon doesn't exist in this generation")
}

menu <- function () {
  writeLines("\n=== Menu ===")
  writeLines("1 - View Pokemon list")
  writeLines("2 - Calculate probability of a catch rate for a given pokemon")
  writeLines("3 - Exit")
  menu <- as.numeric(readline("Choose an option: "))
  if(is.na(menu) || (menu != 1 && menu != 2 && menu != 3)){
    writeLines ("Try again")
    menu()
  }
  else if (menu == 1) {
    print.data.frame(pokeList[,c(1,3)], row.names = FALSE)
    menu()
  }
  else if (menu == 2) {
    pokeChosen <- readline("Pick one Pokemon, by id or by identifier: ")
    
    if(pokeChosen %in% pokeList$pokemon_id) {
      cat("You have chosen: ", pokeList[pokeList$pokemon_id == pokeChosen, "identifier"], ". Good choice!", sep = "")
      pokeproba(pokeChosen)
    } else if (pokeChosen %in% pokeList$identifier){
      cat("You have chosen: ", pokeChosen, ". Good choice! ", sep = "")
      pokeproba(pokeList[pokeList$identifier == pokeChosen, "pokemon_id"])
    } else writeLines ("Try again")
    
    menu()
  }
}