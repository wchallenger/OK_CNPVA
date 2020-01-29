#' Get Simulation Settings
#'
#' @param type character string indicating the settings scenario to retrieve
#'
#' @return list object containing required simulator parameter settings.
#' 
#' @examples
#' setting.default <- GetSettings("default")
#' str(setting.default)
#' 
#' @export
GetSettings <- function(type = "default v2") {
  
  # Original PVA ANALYSIS
  default.v1 <- list(
    #--- Recruitment ---
    rec.alpha = 136,
    rec.beta  = 2400,    # avoids namespace collision with beta() function.
    rec.sigma = 0.53,
    seed.spawners = 35,
    supplementation = 0,
    suppl.fitness = 1,
    # Early juvenile survival during out migration
    S.juv   = 0.44,
    S.juv.sig = 0.09,
    S.osar = 0.025,    
    # Ocean Survival and Maturation and ocean harvest
    surv.ocean = c(
      age.1.2 = 1.00,   #  0.60  Survival from age 1 to age 2 - This is acutally captured in S.osar
      age.2.3 = 0.70,    # Survival from age 1 to age 2
      age.3.4 = 0.80,
      age.4.5 = 0.90
    ),
    # Yearly ocean harvest rate (confirmed with Rishi that this was )
    hr.ocean  = 0.60,   
    # spawning maturation as a series of continuation ratios?
    spawn.prob = c(
      age.2 = 0.043,
      age.3 = 0.26,
      age.4 = 0.72,
      age.5 = 1.00
    ),
    #-- Returns, escapement and in-river harvest
    S.riv = 0.68,         # Adult in-river survival up columbia
    S.riv.sig = 0.15,         # Adult in-river survival up columbia
    hr.river = 0.18
  )
  
  # 2019 PVA UPDATED parameter values
  default.v2 <- default.v1
  default.v2$seed.spawners <- 50
  default.v2$hr.ocean <- 0.25 # last 10 BY
  default.v2$hr.river <- 0.42 # last 10 BY
  default.v2$S.osar = 0.084
  default.v2$S.juv = 0.35
  default.v2$S.riv = 0.94 # last 5 years
  default.v2$S.riv.sig = 0.05 # last 5 yrs
  
  
  if (tolower(type) %in% c("default", "default v1")) {
    if (tolower(type) == "default")
      warning("Two default settings exist, assuming 'default v1'.")
    return(default.v1)
  } else if (tolower(type) %in% c("default v2")) {
    return(default.v2)
  }
  else {
    stop("Setting type is unkown.")
  }
  
  
  
  
  
}
