#=========================================================
#  Optimisation functions
# =========================================================

# Static functions used by the optimisation script
# Used to be a much longer clunkier series of functions repeated for different operatives versions with different numbers of parameters
# Reduced and simplified with help of chatGPT. Tested to still give same answers as before

# A small function to strip the trailing 0s for presentation. Needs as.character as input. Likely to use elsewhere?
trim_zeros <- function(x) sub("^(-?)0\\.", ".", sprintf("%s", as.numeric(x)))

model_names <- c(
  'full',
  'noAct',
  'noInf',
  'noSelect',
  'noActnoInf',
  'noActnoSelect',
  'noInfnoSelect',
  'noActnoInfnoSelect'
)

model_names2 <- c(
  'noKind',
  'noActnoKind',
  'noInfnoKind',
  'noKindnoSelect',
  'noActnoInfnoKind',
  'noActnoKindnoSelect',
  'noInfnoKindnoSelect',
  'noActnoInfnoKindnoSelect',
  'baseline'
)

# =======================================
# OPERATIVE DEFINITIONS of 'operative part' (what differs between optimisation functions for kindness v no kindness - what used to be done using different series of functions)
# =======================================

# WITHOUT kappa for models with noKind
NO_KAPPA <- list(
  name = "no_kappa",
  n_params = 3,
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]), # constrained to between 0,1
      tau2 = exp(pars[3]), # positive only
      kappa = NULL
    )
  }
  # logits = function(df, mod_name) {
  #   # Just the model column - no funny stuff for this 2-par version!
  #   df[[mod_name]]
  # }
)

# WITH kappa for kindness - full model
WITH_KAPPA <- list(
  name = "with_kappa",
  n_params = 4,
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = exp(pars[4])
    )
  }
  # logits = function(df, mod_name) {
  #   # Model column plus kappa * ig - this is the part that's extra from the 2-par version and which models the contribution of Kindness
  #   df[[mod_name]] + df$ig
  # }
)

# =======================================
#  VECTORIZED LIKELIHOOD FUNCTION
# =======================================

get_likelihood <- function(pars, df, mod_name, operative) {
  # -----------------------------
  # 1. Prepare data
  # -----------------------------
  # Drop unused trial levels to prevent empty splits
  # df$trial_id <- droplevels(df$trial_id)
  # df$trial_id <- as.character(df$trial_id)

  # Transform raw parameters into usable values
  params <- operative$transform(pars)

  # split into named list of trials using split (keeps names)
  trials <- split(df, df$trial_id)

  # -----------------------------
  # 2. Compute negative log-likelihood
  # -----------------------------

  #
  nlls <- vapply(
    # like sapply but is safer and faster apparently
    trials,
    function(tr) {
      # ----- get logits
      # Compute logits according to whether it's WITH or NO kappa
      #base_logits <- operative$logits(tr, mod_name)
      # Mask the index places where -Inf and keep for later

      base_logits <- tr[[mod_name]]
      positions <- which(is.finite(base_logits))
      # -------- First softmax
      logits <- base_logits / params$tau1
      # Run soft1 only on finite positions and preserve the -Inf at the other positions
      soft1 <- rep(-Inf, length(logits))
      soft1[positions] <- exp(logits[positions]) / sum(exp(logits[positions]))

      # Add kappa scaling if applicable
      if (!is.null(params$kappa)) {
        idx <- is.finite(soft1)
        soft1[idx] <- soft1[idx] + params$kappa * tr$ig[idx]
      }

      # if (!is.finite(params$tau1) || params$tau1 <= 0) return(Inf)
      # if (!is.finite(params$tau2) || params$tau2 <= 0) return(Inf)
      # if (!is.finite(params$epsilon)) return(Inf)

      #print(str(logits))

      #logits <- logits - max(logits) # A bugfix from chatGPT - Softmax is invariant to adding/subtracting a constant to all logits?

      # Currently big problem is that first softmax should only be applied over the four coherent options, not everything or it turns it to mush

      # -------- Second softmax for choice
      logits2 <- soft1 / params$tau2

      #logits2 <- logits2 - max(logits2)

      soft2 <- exp(logits2) / sum(exp(logits2))

      mpred <- params$epsilon * (1 / 8) + (1 - params$epsilon) * soft2

      # if (any(mpred <= 0)) {
      #   stop("Predicted probability <= 0. trial_id = ", tr$trial_id[1])
      # }

      # if (sum(tr$n) == 0) {
      #   return(0)
      # }

      -sum(log(mpred) * tr$n)
    },
    numeric(1)
  )
  sum(nlls)
}


# =======================================
#  VECTORIZED PREDICTION FUNCTION
# =======================================

get_prediction <- function(pars, df, mod_name, operative) {
  # -----------------------------
  # 1. Prepare data
  # -----------------------------

  # Drop unused trial levels to prevent empty splits
  # df$trial_id <- droplevels(df$trial_id)
  # df$trial_id <- as.character(df$trial_id)

  # Transform raw parameters into usable values
  params <- operative$transform(pars)

  trials <- split(df, df$trial_id)

  preds <- lapply(names(trials), function(tr_id) {
    tr <- trials[[tr_id]]

    # -------- First softmax over logits with tau1 THIS BIT GO BACK TO SAVED VERSION AND COMPARE - SLIGHTLY DIFFERENT STRUCTURE

    # ----- get logits
    # Compute logits according to whether it's WITH or NO kappa
    #base_logits <- operative$logits(tr, mod_name)

    base_logits <- tr[[mod_name]]
    positions <- which(is.finite(base_logits))
    # -------- First softmax
    logits <- base_logits / params$tau1
    # Run soft1 only on finite positions and preserve the -Inf at the other positions
    soft1 <- rep(-Inf, length(logits))
    soft1[positions] <- exp(logits[positions]) / sum(exp(logits[positions]))

    # Add kappa scaling if applicable
    if (!is.null(params$kappa)) {
      idx <- is.finite(soft1)
      soft1[idx] <- soft1[idx] + params$kappa * tr$ig[idx]
    }

    # if (!is.finite(params$tau1) || params$tau1 <= 0) return(Inf)
    # if (!is.finite(params$tau2) || params$tau2 <= 0) return(Inf)
    # if (!is.finite(params$epsilon)) return(Inf)

    #print(str(logits))

    #logits <- logits - max(logits) # A bugfix from chatGPT - Softmax is invariant to adding/subtracting a constant to all logits?

    # Currently big problem is that first softmax should only be applied over the four coherent options, not everything or it turns it to mush

    # -------- Second softmax for choice
    logits2 <- soft1 / params$tau2

    #logits2 <- logits2 - max(logits2)

    soft2 <- exp(logits2) / sum(exp(logits2))

    mpred <- params$epsilon * (1 / 8) + (1 - params$epsilon) * soft2

    # if (any(mpred <= 0)) {
    #   stop("Predicted probability <= 0 in trial ", tr$trial_id[1])
    # }

    data.frame(
      model = mod_name,
      trial_id = tr_id,
      node3 = tr$node3,
      predicted_prob = mpred
    )
  })

  do.call(rbind, preds)
}


# =======================================
#  NORMAL (NON-PARALLEL) MODEL FITTER (don't need parallel, not big enough)
# =======================================

get_optimisation <- function(
  model_names,
  df,
  operative,
  initial_values = rep(1, operative$n_params)
) {
  # ---------------------------
  # A. Optimize each model
  # ---------------------------
  fits <- lapply(model_names, function(mod_name) {
    tryCatch(
      optim(
        par = initial_values,
        fn = get_likelihood,
        df = df,
        mod_name = mod_name,
        operative = operative
      ),
      error = function(e) {
        message("Error in optimization for model ", mod_name, ": ", e$message)
        list(par = rep(NA, operative$n_params), value = NA)
      }
    )
  })
  names(fits) <- model_names

  # ---------------------------
  # B. Build model_fits table (perhaps this is not most efficient structure but let's keep it to fit later analysis)
  # ---------------------------
  tau1 <- sapply(fits, function(x) exp(x$par[1]))
  epsilon <- sapply(fits, function(x) plogis(x$par[2]))
  tau2 <- sapply(fits, function(x) exp(x$par[3]))
  logl <- -sapply(fits, function(x) x$value)

  model_fits <- data.frame(
    model = model_names,
    tau1 = tau1,
    epsilon = epsilon,
    tau2 = tau2,
    logl = logl
  )

  # Add kappa only if 4-parameter model
  if (operative$n_params == 4) {
    model_fits$kappa <- sapply(fits, function(x) exp(x$par[4]))
    n_par <- 4
  } else {
    n_par <- 3
  }

  # Calculate BIC
  model_fits$BIC <- -2 * model_fits$logl + n_par * log(sum(df$n))

  # Format model_fits
  model_fits <- model_fits |>
    mutate(
      tau1 = format(tau1, digits = 3),
      epsilon = format(epsilon, digits = 3),
      tau2 = format(tau2, digits = 3),
      logl = format(logl, digits = 4),
      BIC = format(BIC, digits = 4)
    )

  if (operative$n_params == 4) {
    model_fits <- model_fits |>
      mutate(kappa = format(kappa, digits = 3))
  }

  # ---------------------------
  # C. Build predictions table: a prediction for each variable in each condition for each model
  # ---------------------------
  predictions <- do.call(
    rbind,
    lapply(model_names, function(mod_name) {
      pars <- fits[[mod_name]]$par
      if (!any(is.na(pars))) {
        get_prediction(
          pars = pars,
          df = df,
          mod_name = mod_name,
          operative = operative
        )
      }
    })
  )

  # ---------------------------
  # D. Return
  # ---------------------------

  list(
    model_fits = model_fits,
    predictions = predictions
  )
}
