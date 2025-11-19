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

# THIS WAS A HILARIOUSLY LONG AND CLUNKY VERSION AND STILL GAVE THE SAME ANSWERS AS THE ONE I WROTE BEFORE
# TO DO - 1) PROFVIS ON ONE DF, 2) REPLACE VAPPLY NNLS WITH FOR LOOP AS BEFORE

# WITHOUT kappa for models with noKind
NO_KAPPA <- list(
  name = "no_kappa",
  n_params = 2,
  transform = function(pars) {
    list(
      epsilon = plogis(pars[1]), # constrained to between 0,1
      tau = exp(pars[2]), # positive only
      kappa = NULL
    )
  },
  logits = function(df, params, mod_name) {
    # Just the model column - no funny stuff for this 2-par version!
    df[[mod_name]]
  }
)

# WITH kappa for kindness - full model
WITH_KAPPA <- list(
  name = "with_kappa",
  n_params = 3,
  transform = function(pars) {
    list(
      epsilon = plogis(pars[1]),
      tau = exp(pars[2]),
      kappa = exp(pars[3])
    )
  },
  logits = function(df, params, mod_name) {
    # Model column plus kappa * ig - this is the part that's extra from the 2-par version and which models the contribution of Kindness
    df[[mod_name]] + (params$kappa * df$ig)
  }
)

# =======================================
#  VECTORIZED LIKELIHOOD FUNCTION
# =======================================

get_likelihood <- function(pars, df, mod_name, operative) {
  # ---- Defensive check: pars must be numeric ----
  if (!is.numeric(pars)) {
    # show minimal info to debug
    msg <- paste0(
      "get_likelihood: 'pars' not numeric. class(pars) = ",
      paste(class(pars), collapse = "/"),
      ", length = ",
      length(pars),
      ". Trying safe coercion..."
    )
    message(msg)

    # try safe coercion if sensible
    pars_num <- suppressWarnings(as.numeric(pars))
    if (all(!is.na(pars_num)) && length(pars_num) == length(pars)) {
      message("get_likelihood: coercion to numeric succeeded; proceeding.")
      pars <- pars_num
    } else {
      stop(
        "get_likelihood: 'pars' cannot be safely coerced to numeric. ",
        "Inspect 'pars' when calling optim: class(pars) = ",
        paste(class(pars), collapse = "/"),
        "; head(pars) = ",
        paste(utils::head(as.character(pars), 10), collapse = ", ")
      )
    }
  }
  # -----------------------------
  # 1. Prepare data
  # -----------------------------
  # Ungroup in case df is grouped_df (from dplyr)
  #df <- ungroup(df)

  # Drop unused trial levels to prevent empty splits
  df$trial_id <- droplevels(df$trial_id)
  df$trial_id <- as.character(df$trial_id)

  # Transform raw parameters into usable values
  params <- operative$transform(pars)

  # Split data into list of trials
  # trials <- df |>
  #   group_by(trial_id) |>
  #   group_split(.keep = TRUE)

  # split into named list of trials using split (keeps names)
  trials <- split(df, df$trial_id)

  # -----------------------------
  # 2. Compute negative log-likelihood
  # -----------------------------

  # Test version
  nlls <- vapply(
    trials,
    function(tr) {
      # Debug: print trial id if needed
      # message("Trial ", tr$trial_id[1])

      # Compute logits
      logits <- operative$logits(tr, params, mod_name) / params$tau

      if (any(is.na(logits))) {
        stop(
          "NA values in logits. trial_id = ",
          tr$trial_id[1],
          "; params = ",
          paste(unlist(params), collapse = ",")
        )
      }

      logits <- logits - max(logits) # A bugfix from chatGPT - Softmax is invariant to adding/subtracting a constant to all logits?
      soft <- exp(logits)
      if (any(is.na(soft))) {
        stop(
          "NA values in exp(logits). trial_id = ",
          tr$trial_id[1],
          "; logits = ",
          paste(logits, collapse = ",")
        )
      }

      soft <- soft / sum(soft)
      if (any(is.na(soft))) {
        stop(
          "NA in softmax normalisation. trial_id = ",
          tr$trial_id[1],
          "; soft = ",
          paste(soft, collapse = ",")
        )
      }

      mpred <- params$epsilon * (1 / 8) + (1 - params$epsilon) * soft

      if (any(is.na(mpred))) {
        stop(
          "mpred contains NA. trial_id = ",
          tr$trial_id[1],
          "; epsilon=",
          params$epsilon,
          "; tau=",
          params$tau,
          "; soft = ",
          paste(soft, collapse = ",")
        )
      }

      if (any(mpred <= 0)) {
        stop("Predicted probability <= 0. trial_id = ", tr$trial_id[1])
      }

      if (sum(tr$n) == 0) {
        return(0)
      }

      -sum(log(mpred) * tr$n)
    },
    numeric(1)
  )

  # Vectorized computation using vapply
  # nlls <- vapply(
  #   names(trials),
  #   function(tr_id) {
  #     tr <- trials[[tr_id]]
  #     # Compute logits from this model's structure
  #     logits <- operative$logits(tr, params, mod_name) / params$tau
  #
  #     # Softmax over options in this trial
  #     soft <- exp(logits)
  #     soft <- soft / sum(soft)
  #
  #     # Final choice probability
  #     mpred <- params$epsilon * (1 / 8) + (1 - params$epsilon) * soft
  #
  #     # Instead of clipping, ERROR if invalid
  #     if (any(mpred <= 0)) {
  #       stop("Predicted probability <= 0 in trial ", tr$trial_id[1])
  #     }
  #
  #     # Return negative log-likelihood
  #     # contribution (0 if no observations)
  #     if (sum(tr$n) == 0) {
  #       return(0)
  #     }
  #     -sum(log(mpred) * tr$n)
  #     #-sum(log(mpred) * tr$n)
  #     #-sum(log(mpred) * tr$n) * (sum(tr$n) > 0) # second part is an indicator function which returns FALSE if no data in this trial
  #   },
  #   numeric(1)
  # )
  sum(nlls)
}


# =======================================
#  VECTORIZED PREDICTION FUNCTION
# =======================================

get_prediction <- function(pars, df, mod_name, operative) {
  # -----------------------------
  # 1. Prepare data
  # -----------------------------
  # Ungroup
  #df <- ungroup(df)

  # Drop unused trial levels to prevent empty splits
  df$trial_id <- droplevels(df$trial_id)
  df$trial_id <- as.character(df$trial_id)

  # Transform raw parameters into usable values
  params <- operative$transform(pars)

  trials <- split(df, df$trial_id)
  # Split data into list of trials
  # trials <- df |>
  #   group_by(trial_id) |>
  #   group_split(.keep = TRUE)

  preds <- lapply(names(trials), function(tr_id) {
    tr <- trials[[tr_id]]
    logits <- operative$logits(tr, params, mod_name) / params$tau

    # Stabilise first
    logits <- logits - max(logits) # A bugfix from chatGPT - Softmax is invariant to adding/subtracting a constant to all logits?
    # Softmax over options in this trial
    soft <- exp(logits)
    soft <- soft / sum(soft)

    # Incorporate epsilon - a parameter to allow for random/noisy choice of any of the eight variable answers
    mpred <- params$epsilon * (1 / 8) + (1 - params$epsilon) * soft

    if (any(mpred <= 0)) {
      stop("Predicted probability <= 0 in trial ", tr$trial_id[1])
    }

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
  epsilon <- sapply(fits, function(x) plogis(x$par[1]))
  tau <- sapply(fits, function(x) exp(x$par[2]))
  logl <- -sapply(fits, function(x) x$value)

  model_fits <- data.frame(
    model = model_names,
    epsilon = epsilon,
    tau = tau,
    logl = logl
  )

  # Add kappa only if 3-parameter model
  if (operative$n_params == 3) {
    model_fits$kappa <- sapply(fits, function(x) exp(x$par[3]))
    n_par <- 3
  } else {
    n_par <- 2
  }

  # Calculate BIC
  model_fits$BIC <- -2 * model_fits$logl + n_par * log(sum(df$n))

  # Format model_fits
  model_fits <- model_fits |>
    mutate(
      epsilon = format(epsilon, digits = 3),
      tau = format(tau, digits = 3),
      logl = format(logl, digits = 4),
      BIC = format(BIC, digits = 4)
    )

  if (operative$n_params == 3) {
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
