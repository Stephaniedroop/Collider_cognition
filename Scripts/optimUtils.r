##################################################################
######## Optimisation functions ######################################

# Static functions used by the opimisation script

# A small function to strip the trailing 0s for presentation. Needs as.character as input. LIkely to use elsewhere?
trim_zeros <- function(x) sub("^(-?)0\\.", ".", sprintf("%s", as.numeric(x)))

# TO DO: reduce the functions down to be ureused whether 3 or 2 parameters

####### ----------------1.  The three-par versions ---------------------- 

# Function to get the model likelihood. The THREE PAR version
model_likelihood <- function(pars, df, mod_name)
{
  # 
  epsilon <- plogis(pars[1])  # exp(pars[1])/(1+exp(pars[1])) # 
  tau <- exp(pars[2]) # exp(pars[2])/(1+exp(pars[2])) #   
  kappa <- exp(pars[3])
  tt <- unique(df$trial_id) 
  
  nlls <- rep(NA, length(tt))
  for (i in 1:length(tt))
  {
    n <- df$n[df$trial_id==tt[i]] 
    mod_raw <- df[[mod_name]][df$trial_id==tt[i]] 
    
    # A distance measure which gets parametrised by kappa CHOOSE ONE AND COMMENT THE OTHER
    # Either include Known: this lets kappa scale 1 when Known==T and there is no ig
    # dist <- df$ig[df$trial_id==tt[i]] + df$Known[df$trial_id==tt[i]]
    # kappa_contrib <- kappa*(dist)
    
    # OR: just use ig, so kappa scales with information gain only
    dist <- df$ig[df$trial_id==tt[i]]
    kappa_contrib <- ifelse(dist > 0, kappa * (1 + dist), 0)
    
    # dist[is.na(dist)] <- 0
    
    # Model predictions
    mpred <- epsilon * (1/8) + (1-epsilon) * (exp( (mod_raw + kappa_contrib)/tau) / sum(exp( (mod_raw + kappa_contrib)/tau)))
    nlls[i] <-  -sum(log(mpred)*n) # Get likelihood for this trial
  }
  
  sum(nlls) # Return the total likelihood
}



generate_predictions <- function(mod_name, df, pars) {
  epsilon <- plogis(pars[1]) # exp(pars[1])/(1+exp(pars[1])) #  
  tau <- exp(pars[2]) #exp(pars[2])/(1+exp(pars[2])) # 
  kappa <- exp(pars[3])
  tt <- unique(df$trial_id)
  
  do.call(rbind, lapply(tt, function(t_id) {
    trial_rows <- df$trial_id == t_id 
    mod_raw <- df[[mod_name]][trial_rows]
    
    # A distance measure which will get parametrised by kappa
    # Like above, choose one and comment the other
    # Either include Known: this lets kappa scale 1 when Known==T and there is no ig
    # dist <- df$ig[trial_rows] + df$Known[trial_rows]
    # kappa_contrib <- kappa*(dist)
    
    # Or just ig
    dist <- df$ig[trial_rows]
    kappa_contrib <- ifelse(dist > 0, kappa * (1 + dist), 0)
    
    #dist[is.na(dist)] <- 0
    
    # Model predictions
    mpred <- epsilon * (1/8) + (1-epsilon) * (exp( (mod_raw + kappa_contrib)/tau) / sum(exp( (mod_raw + kappa_contrib)/tau)))
    
    data.frame(
      model = mod_name,
      trial_id = t_id,
      node3 = df$node3[trial_rows],
      predicted_prob = mpred
    )
  }))
}

optimize_models <- function(model_names, df, initial_values = c(1, 1, 1)) { 
  optimize_single <- function(mod_name) {
    result <- tryCatch({
      optim(par = initial_values, 
            fn = model_likelihood, 
            df = df, 
            mod_name = mod_name)
    }, error = function(e) {
      message("Error in optimization for model ", mod_name, ": ", e$message)
      return(list(par = c(NA, NA, NA), value = NA))
    })
    return(result)
  }
  
  out <- lapply(model_names, optimize_single)
  names(out) <- model_names
  
  mfs <- data.frame(
    model = names(out),
    epsilon = plogis(sapply(out, function(x) x$par[1])), 
    tau = exp(sapply(out, function(x) x$par[2])), # 
    kappa = exp(sapply(out, function(x) x$par[3])),
    logl = -sapply(out, function(x) x$value)
  ) |>
    mutate(BIC = -2 * logl + 3 * log(sum(df$n)))
  
  # To actually generate predictions 
  predictions <- do.call(rbind, lapply(names(out), function(mod_name) {
    if(!any(is.na(out[[mod_name]]$par))) {
      generate_predictions(
        mod_name = mod_name,
        df = df,
        pars = out[[mod_name]]$par
      )
    }
  }))
  
  list(
    model_fits = mfs |> 
      mutate(epsilon = format(epsilon, digits=3), tau = format(tau, digits=3), kappa = format(kappa, digits=3),
             logl = format(logl, digits=4), BIC = format(BIC, digits=4)),
    predictions = predictions
  )
}



# ------------------ 2. The two-par version ----------------------------


# Function to get the model likelihood.
model_likelihood2 <- function(pars, df, mod_name)
{
  # 
  epsilon <- plogis(pars[1]) # exp(pars[1])/(1+exp(pars[1])) #  
  tau <-  exp(pars[2]) # exp(pars[2])/(1+exp(pars[2])) # 
  #kappa <- exp(pars[3])
  tt <- unique(df$trial_id) 
  
  nlls <- rep(NA, length(tt))
  for (i in 1:length(tt))
  {
    n <- df$n[df$trial_id==tt[i]] 
    mod_raw <- df[[mod_name]][df$trial_id==tt[i]] 
    #mod_raw[is.na(mod_raw)] <- 0 
    
    #dist <- df$tv[df$trial_id==tt[i]] 
    #dist[is.na(dist)] <- 0
    #dist <- df$ig[df$trial_id==tt[i]] 
    #dist[is.na(dist)] <- 0
    #kappa_contrib <- ifelse(dist == 0, 0, kappa*(dist+1))
    #kappa_contrib <- kappa*(dist+1)
    
    #kappa_contrib <- ifelse(dist == 0, 0, kappa*(dist+1))
    #kappa_contrib <- kappa*(dist)
    
    mpred <- epsilon * (1/8) + (1-epsilon) * (exp((mod_raw)/tau) / sum(exp((mod_raw)/tau)))
    
    nlls[i] <-  -sum(log(mpred)*n) # Get likelihood for this trial
  }
  
  sum(nlls) # Return the total likelihood
}


generate_predictions2 <- function(mod_name, df, pars) {
  epsilon <- plogis(pars[1])  #exp(pars[1])/(1+exp(pars[1])) # 
  tau <- exp(pars[2]) #exp(pars[2])/(1+exp(pars[2])) # 
  #kappa <- exp(pars[3])
  tt <- unique(df$trial_id)
  
  do.call(rbind, lapply(tt, function(t_id) {
    trial_rows <- df$trial_id == t_id 
    mod_raw <- df[[mod_name]][trial_rows]
    #mod_raw[is.na(mod_raw)] <- 0
    #dist <- df$ig[trial_rows] + df$Known[trial_rows]
    #dist[is.na(dist)] <- 0
    
    mpred <- epsilon * (1/8) + (1-epsilon) * (exp((mod_raw)/tau) / sum(exp((mod_raw)/tau)))
    
    data.frame(
      model = mod_name,
      trial_id = t_id,
      node3 = df$node3[trial_rows],
      predicted_prob = mpred
    )
  }))
}



optimize_models2 <- function(model_names, df, initial_values = c(1, 1)) { 
  optimize_single <- function(mod_name) {
    result <- tryCatch({
      optim(par = initial_values, 
            fn = model_likelihood2, 
            df = df, 
            mod_name = mod_name)
    }, error = function(e) {
      message("Error in optimization for model ", mod_name, ": ", e$message)
      return(list(par = c(NA, NA), value = NA))
    })
    return(result)
  }
  
  out <- lapply(model_names, optimize_single)
  names(out) <- model_names
  
  mfs <- data.frame(
    model = names(out),
    epsilon = plogis(sapply(out, function(x) x$par[1])), #plogis(logodd) gives prob
    tau = exp(sapply(out, function(x) x$par[2])),
    #kappa = exp(sapply(out, function(x) x$par[3])),
    logl = -sapply(out, function(x) x$value)
  ) |>
    mutate(BIC = -2 * logl + 2 * log(sum(df$n)))
  
  # To actually generate predictions 
  predictions <- do.call(rbind, lapply(names(out), function(mod_name) {
    if(!any(is.na(out[[mod_name]]$par))) {
      generate_predictions2(
        mod_name = mod_name,
        df = df,
        pars = out[[mod_name]]$par
        # epsilon = plogis(sapply(out, function(x) x$par[1])),
        # tau = plogis(sapply(out, function(x) x$par[2])),
        # kappa = exp(sapply(out, function(x) x$par[3])),
      )
    }
  }))
  
  list(
    model_fits = mfs |> 
      mutate(epsilon = format(epsilon, digits=3), tau = format(tau, digits=3),
             logl = format(logl, digits=4), BIC = format(BIC, digits=4)),
    predictions = predictions
  )
}