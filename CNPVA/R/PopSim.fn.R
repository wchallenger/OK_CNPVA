#' Chinook Population Simulator
#'
#' @param sim.settings list object containing simulation parameter values, see details for more information.
#' @param n.sim numeric containing number of simulation runs.
#' @param verbose logical indicating whether status messages should be displayed.
#'
#' @return a list object with simulation results.
#'
#' @examples
#' sim.result <- PopSim(sim.settings = GetSettings("default"), n.sim=5) 
#' str(sim.result)
#' 
#' @export
PopSim <- function(sim.settings = NULL, years = 2020:2060, n.sim = 10000, verbose=FALSE) {
  
  # Rickers Spawner/Recruitment Function 
  SpawnRec <- function(spawners, a, b, sigma) {
    # expected number of spawner recruits without stochastic processes
    expected  <-   a*spawners*exp(-spawners/b)
    # realized number of spawner recruits with log normal error
    realized <- expected * exp(rnorm(n = length(spawners), mean=0, sd=sigma))
    return(realized)
  }
  
  
  
  # # Rickers Spawner/Recruitment Function 
  # SpawnRec <- function(spawners, a, b, sigma) {
  #   # expected number of spawner recruits without stochastic processes
  #   expected  <-   r2(spawners, a = 4.387e+00, b = 1.198e-05)
  #   # realized number of spawner recruits with log normal error
  #   realized <- expected * exp(rnorm(n = length(spawners), mean=0, sd=sigma))
  #   return(realized)
  # }
  
  # browser()
  # Helper: Stochastic Survival Function 
  # Default function used to draw random survival rates, not used in the 
  # actual VPA analysis.  Caution should be taken as the distribution is 
  # can become truncated at the tails.  Alternatively, 
  StochSurv <- function(n, mean, sd) {
    # Yearly survival with random error
    x <- rnorm(n=n, mean=mean, sd=sd)
    # Constrain maximum value to 1
    x <- apply(cbind(x, 1), 1, min)
    # Constrain minimum value to 0
    x <- apply(cbind(x, 0), 1, max)
    
    # Alternative Formulation: anti-logit
    # x <- plogis(rnorm(n=n, mean=mean, sd=sd))
    return(x)
  }
  
  
  
  
  if (is.null(sim.settings)) sim.settings <- GetSettings("default")
  

  # Check Setting Variables -------------------------------------------------
  # Ensure that we have all of the required setting varialbles
  setting.parms <- c(
    # Recruitment
    "rec.alpha","rec.beta", "rec.sigma", "seed.spawners", 
    "supplementation", "suppl.fitness",
    
    # Early juvenile survival
    "S.juv", "S.juv.sig", "S.osar", 
    
    # Adult ocean survival
    "surv.ocean", "hr.ocean",
    
    # Return migration
    "spawn.prob", "S.riv", "S.riv.sig", "hr.river"
  )
  
  check <- setting.parms %in% names(sim.settings)
  if (!all(check)) 
    stop("Missing the following simulation parameter settings: ", paste(setting.parms[!check], collapse=", "))
  
  attach(sim.settings)
 
  # browser()
   #--- Default Settings ---
  if (!exists('years')) years <- 2001:2050
  if (!exists('ages')) ages <- 1:5
  
  #--- Derived Parameters ---
  n.yrs <- length(years)
  n.age <- length(ages)
  # --- Parameter Initialization ----
  
  # Cohort Specific arrays
  temp <- array(
    data = NA, 
    dim =c(n.yrs, n.age - 1, n.sim), 
    dimnames = list(
      paste("cohort", years, sep=".") ,
      paste("age", ages[-1], sep="."), 
      paste("sim", seq_len(n.sim), sep=".")
    )
  )
  N.adult <- C.ocean <- R.adult <- C.river <- E.river <- temp
  rm(temp)
  
  N.juv <- array(
    data = NA,
    dim = c(n.yrs, 3, n.sim),
    dimnames = list(
      paste("cohort", years, sep=".") ,
      c("spawners", "spawn.rec", "juvenile.rec"), 
      paste("sim", seq_len(n.sim), sep=".")
    )
  )
  
  # Year specific arrays
  temp <- array(
    data = NA,
    dim = c(n.yrs, n.age - 1, n.sim),
    dimnames = list(
      as.character(years) ,
      paste("age", ages[-1], sep="."), 
      paste("sim", seq_len(n.sim), sep=".")
    )
  )
  esc <- pop <- temp
  
  
  #--- Population Simulation ---
  # The simulator uses for loops to work throught time steps, but vectorization
  # to handle the number of simulations, as large for loops tend to be 
  # comptuationally inefficient in the R computing environment.
  
  for (y in seq_len(n.yrs)) {
    
    cohort <- paste0("cohort.", years[y])
    yr <- as.character(years[y])
    if (verbose) message(cohort)
    if (y <= length(ages)) {
      # initialize first 5 years with 35 spawners
      N.juv[cohort, "spawners", ] <- seed.spawners
    } else {
   
      # Create index cohort years and respective age categories corresponding to
      # the current calendar year.  This will be used to extract the correct
      # cohort-specific escapement, and abundance records records.
      
      cohort.age <- cbind(
        paste0("cohort.", years[seq( y-2, y-length(ages))]),  
        paste0("age.", ages[-1])
      )
      # Compute total spawners for the calendar year
      N.juv[cohort, "spawners", ] <- apply(E.river, 3, function(x) {sum(x[cohort.age])})
      
      # Extract calendar year escapement by age category
      esc[yr, , ] <- apply(E.river, 3, function(x) {x[cohort.age]})
      
      # Extract population size
      pop[yr, , ] <- apply(N.adult, 3, function(x) {x[cohort.age]})
    }
   
    # Realized spawner recuits (Expected + Stochastic Error)
    N.juv[cohort, "spawn.rec", ] <- SpawnRec(N.juv[cohort,'spawners', ], rec.alpha, rec.beta, rec.sigma) +  supplementation*suppl.fitness
    
    #  Juvenile early ocean and early ocean to adult 2
    N.juv[cohort, "juvenile.rec", ] <- N.juv[cohort, "spawn.rec", ] * S.osar * StochSurv(n.sim, mean=S.juv, sd=S.juv.sig) 
    
    # Juvenile early ocean recruits to Age 2 adults 
    N.adult[cohort, 'age.2', ] <-  N.juv[cohort, "juvenile.rec", ] * surv.ocean['age.1.2']
    
    for (a in 2:5) {
      age <- paste0('age.', a)
      
      #--- Maturation to the next age category
      # Survival and maturation  to the next age category: 
      #            N_a+1 = (N_a - C_a - R_a)S_{a>>a+1}
      # where N_a is number of adults in the current age category, C_a is the 
      # ocean catch taken from the current age category, R_a are the number
      # of adults returning to span, and S_a>>a+1 is the probability of surviving
      # to the next age class.
      
      # Catch
      C.ocean[cohort, age, ] <- N.adult[cohort, age, ] * hr.ocean
      # Adults returning to spawn
      R.adult[cohort, age, ] <-  (N.adult[cohort, age, ] - C.ocean[cohort, age, ])*spawn.prob[age]
      
      # Maturation to next age category
      if (a < 5) {
        age1plus <- paste0('age.', a + 1)
        N.adult[cohort, age1plus, ] <-  (N.adult[cohort, age, ] - C.ocean[cohort, age, ] - R.adult[cohort, age, ])*surv.ocean[paste('age', a, a+1, sep=".")]
      }
    }
    
    #--- Escapement ---
    # For individuals in the cohort spawning we need to determine how many are
    # harvested from in-river fisheries, then how many of the remaining
    # survive damn passage up the columbia to spawn.
    
    # In-river harvest
    C.river[cohort, , ] <-  R.adult[cohort, , ]*hr.river 
    
   
    # Escapement 
    if (class(S.riv) == "numeric")  {
      # browser()
      E.river[cohort, , ] <- (R.adult[cohort, , ] - C.river[cohort, , ])*StochSurv(n.sim, mean = S.riv, sd = S.riv.sig)
    } else if (class(S.riv) == "lm") {
      # we predict survival rate based on a linear regression model provided
      # in settings.  Need to check what scale the Year covariate is on
      # to ensure we generate the correct prediction
       # browser()
      pred <- predict(S.riv, data.frame(Year = 2000 + y - 1 + seq(2, n.age)), se.fit = TRUE, interval="prediction")
      surv.logit <- matrix(pred$fit[, 'fit'], nrow=n.age-1, ncol=n.sim) + 
        matrix(rnorm(n = (n.age-1)*n.sim , mean = 0, sd = pred$se.fit), nrow=n.age-1, ncol=n.sim)
      surv <- plogis(surv.logit)
      E.river[cohort, , ] <- (R.adult[cohort, , ] - C.river[cohort, , ])*surv
    }
  }
  

  # browser()
  # Post-Processing ---------------------------------------------------------
  # @TODO - Exctract yearly escapement and population numbers from the cohort
  # specific arrays.
  
  
  detach(sim.settings)
  
  spawners <-  N.juv[ ,"spawners", ]  # number of spawners for a particular cohort
  rownames(spawners) <- str_extract(dimnames(spawners)[[1]], "[:digit:]{4}$")

  
  return(
    list(
      spawners = spawners,     # N.juv[ ,"spawners", ],
      spawner.recruits = N.juv[ , "spawn.rec", ], 
      juvenile.recruits = N.juv[ , "juvenile.rec", ],
      escapement = esc,
      adult.pop = pop
    )
  )
}
