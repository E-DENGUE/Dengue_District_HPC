source('./R/99_load.R')


args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])

all.mods <- list('mod1' = mod1, 'mod2' = mod2, 'mod3' = mod3,'mod4'=mod0)

#j=1
#k=1


 #modN_extract <- as.numeric(str_match(names(all.mods)[k], "mod(\\d+)")[1, 2])
 
  #mod1 <- inla_spacetime_mod(vintage_date = date.test2[j], formula1 = all.mods[[k]], modN=modN_extract ) 


#Or you can use for loop to run in your computer
for (j in 1:length(date.test2)) {
 for (k in 1:4) {
    tryCatch({
      modN_extract <- as.numeric(str_match(names(all.mods)[k], "mod(\\d+)")[1, 2])
      mod1 <- inla_spacetime_mod(vintage_date = date.test2[j], formula1 = all.mods[[k]], modN = modN_extract)
    }, error = function(e) {
      message(paste("Skipping iteration j =", j, "k =", k, "due to error:", e$message))
    })
  }
}


library(stringr)

# your list of test dates
q <- c("2023-08-01","2023-07-01","2023-04-01",
       "2021-10-01","2022-06-01")

# pick k = 2 once
k <- 3

# if you still want to extract the mod number from the name:
modN_extract <- as.numeric(str_match(names(all.mods)[k], "mod(\\d+)")[1, 2])

# loop over q only
for (j in seq_along(q)) {
  tryCatch({
    mod1 <- inla_spacetime_mod(
      vintage_date = as.Date(q[j]),
      formula1     = all.mods[[k]],
      modN         = modN_extract
    )
    # do whatever you need with mod1 here...
  }, error = function(e) {
    message(sprintf("Skipping date %s due to error: %s", q[j], e$message))
  })
}

