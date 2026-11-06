# =============================================================================
# Recall reliability in retrospective flood surveys and its propagation
# into flood damage models (FDMs)
#
# Tejada Jr., A., Kawasaki, A., Di Baldassarre, G., Jaranilla-Sanchez, P.A.,
# Ohara, M.
#
# -----------------------------------------------------------------------------
# WHAT THIS SCRIPT IS
# A self-contained, runnable demonstration of the full analysis reported in the
# manuscript, organised by research question. It reproduces every statistic and
# figure type in the paper.
#
# WHAT IT DOES NOT CONTAIN
# The household survey data. Those responses can identify participating
# households and cannot be released under the terms of participant consent.
# The script therefore generates a SYNTHETIC dataset with the same structure,
# variable names and missingness pattern as the real survey, so the analysis
# runs end to end and every step can be inspected. Numerical results from the
# synthetic data will NOT match the manuscript; the code path is identical.
#
# To run on the real data, replace simulate_survey() with a read of the
# two-wave file and keep everything downstream unchanged.
#
# Licence: MIT
# =============================================================================

## ---- 0. SETUP --------------------------------------------------------------
# All dependencies are open-source and available from CRAN.
pkgs <- c("dplyr", "tidyr", "purrr", "tibble", "ggplot2", "irr", "irrCAC", "lme4")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

set.seed(2026)

EVENTS      <- c("Ondoy" = 2009, "Ulysses" = 2020, "Karina" = 2024)
YRS_ELAPSED <- c("Ondoy" = 16, "Ulysses" = 5, "Karina" = 1)
N_WAVE1     <- 912   # full first-wave sample
N_RETEST    <- 84    # re-interviewed subsample
AGREE_TOL   <- 0.10  # +/-10% tolerance for continuous agreement

theme_set(theme_minimal(base_size = 12) +
          theme(panel.grid.minor = element_blank(),
                axis.title = element_text(face = "bold"),
                legend.position = "bottom"))

## ---- 1. SYNTHETIC SURVEY ----------------------------------------------------
# Structure mirrors the real instrument: per-event flood characteristics,
# damage costs and recovery durations, plus time-invariant household and
# dwelling attributes. Reliability is induced by adding wave-2 noise whose
# size depends on the variable type, reproducing the qualitative pattern
# reported in the paper.

CONTINUOUS <- c("Depth", "FloodDuration", "Total_Damage_Cost_PHP",
                "Mud_Cleanup_Cost_PHP", "Mud_Cleanup_Days",
                "Return_to_Work_Days", "gov_aid_PHP",
                "Head_Age", "Family_Size", "N_Income_Earners",
                "Total_Household_Income", "savings_PHP",
                "FirstFloor_Height", "Plinth_Height", "Floors", "houseprice")

CATEGORICAL <- c("House_Classification", "Wall_Material", "Roof_Material",
                 "Income_Classification", "HH_Class_Education", "Head_Gender",
                 "Wall_Damage", "Roof_Damage", "Floor_Damage",
                 "savings_binary", "gov_aid_binary", "Evacuated_Before")

# Noise scale by variable: smaller = more reliably recalled.
noise_scale <- function(v, event) {
  base <- dplyr::case_when(
    v %in% c("Depth", "Plinth_Height", "FirstFloor_Height", "Floors") ~ 0.03,
    v %in% c("FloodDuration")                                        ~ 0.08,
    grepl("Days$", v)                                                ~ 0.12,
    grepl("PHP$|Income|savings|houseprice", v)                       ~ 0.25,
    TRUE                                                             ~ 0.10)
  # Salience: the severe 2009 event is recalled no worse than the minor 2024 one
  base * dplyr::case_when(event == "Ondoy" ~ 1.00,
                          event == "Ulysses" ~ 1.05,
                          TRUE ~ 1.15)
}

simulate_survey <- function() {
  hh <- tibble::tibble(
    HHID        = sprintf("H%04d", 1:N_WAVE1),
    Head_Age    = round(rnorm(N_WAVE1, 48, 13)),
    Family_Size = pmax(1, rpois(N_WAVE1, 3.6)),
    N_Income_Earners      = pmax(1, rpois(N_WAVE1, 2)),
    Total_Household_Income = round(rlnorm(N_WAVE1, log(35000), .7)),
    savings_PHP = round(rlnorm(N_WAVE1, log(20000), 1.5)),
    houseprice  = round(rlnorm(N_WAVE1, log(500), 1.1)),
    FirstFloor_Height = round(runif(N_WAVE1, 0, 1.2), 2),
    Plinth_Height     = round(runif(N_WAVE1, 0, .6), 2),
    Floors            = sample(1:3, N_WAVE1, TRUE, c(.3, .57, .13)),
    House_Classification = sample(c("Permanent","Semi-permanent"), N_WAVE1, TRUE, c(.8,.2)),
    Wall_Material        = sample(as.character(1:5), N_WAVE1, TRUE),
    Roof_Material        = sample(as.character(1:5), N_WAVE1, TRUE),
    Income_Classification = sample(c("low","middle","high"), N_WAVE1, TRUE, c(.27,.65,.08)),
    HH_Class_Education    = sample(c("low","medium","high"), N_WAVE1, TRUE, c(.31,.14,.55)),
    Head_Gender           = sample(c("M","F"), N_WAVE1, TRUE))

  per_event <- purrr::map_dfr(names(EVENTS), function(ev) {
    depth <- switch(ev, Ondoy = rgamma(N_WAVE1, 4, 1.15),
                        Ulysses = rgamma(N_WAVE1, 3, 1.75),
                        rgamma(N_WAVE1, 2.5, 2.1))
    dmg   <- pmin(1, 0.034 * sqrt(depth) * exp(rnorm(N_WAVE1, 0, .4)))
    tibble::tibble(
      HHID  = hh$HHID, Event = ev,
      Depth = round(depth, 2),
      FloodDuration = pmin(4, rpois(N_WAVE1, ifelse(ev == "Ondoy", 2, 1))),
      Total_Damage_Cost_PHP = round(dmg * hh$houseprice * 1000),
      Mud_Cleanup_Cost_PHP  = round(rlnorm(N_WAVE1, log(3000), 1.2)),
      Mud_Cleanup_Days      = rpois(N_WAVE1, 5),
      Return_to_Work_Days   = rpois(N_WAVE1, 8),
      gov_aid_PHP           = ifelse(runif(N_WAVE1) < .05, round(rlnorm(N_WAVE1, log(3000), 1)), 0),
      Wall_Damage   = ifelse(depth > 1.0, "Yes", "No"),
      Roof_Damage   = ifelse(depth > 2.2, "Yes", "No"),
      Floor_Damage  = ifelse(depth > 0.4, "Yes", "No"),
      savings_binary   = sample(c("Yes","No"), N_WAVE1, TRUE),
      gov_aid_binary   = ifelse(gov_aid_PHP > 0, "Yes", "No"),
      Evacuated_Before = sample(c("Yes","No"), N_WAVE1, TRUE, c(.6,.4)),
      HDR = dmg)
  })
  dplyr::left_join(per_event, hh, by = "HHID")
}

# Wave 2: re-interview a random subsample, adding recall noise.
simulate_wave2 <- function(w1, ids) {
  # Household-level recall tendency: some respondents answer consistently
  # across the whole questionnaire and others do not. Without this term the
  # noise is independent across households, the HHID variance in the RQ2
  # model is exactly zero, and glmer reports a singular fit.
  hh_factor <- stats::setNames(exp(rnorm(length(ids), 0, 0.45)),
                               as.character(ids))
  w1 %>%
    dplyr::filter(HHID %in% ids) %>%
    dplyr::mutate(hh_f = unname(hh_factor[as.character(HHID)])) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(intersect(CONTINUOUS, names(.))),
      ~ { s <- noise_scale(dplyr::cur_column(), Event) * hh_f
          pmax(0, .x * exp(rnorm(dplyr::n(), 0, s))) })) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(intersect(CATEGORICAL, names(.))),
      ~ { flip <- runif(dplyr::n()) < pmin(0.5, 0.05 * hh_f)
          ifelse(flip, sample(unique(.x), dplyr::n(), TRUE), .x) })) %>%
    dplyr::select(-hh_f)
}

wave1     <- simulate_survey()
retest_id <- sample(unique(wave1$HHID), N_RETEST)
wave2     <- simulate_wave2(wave1, retest_id)
w1_sub    <- dplyr::filter(wave1, HHID %in% retest_id)

## =============================================================================
## RQ1  How reliable are the recalled variables, by item type and flood event?
## =============================================================================
# Two complementary measures:
#   - chance-corrected reliability: ICC(2,1) for continuous, Gwet's AC1 for
#     categorical. Both bounded on a common scale.
#   - percentage agreement: the share of response pairs reproduced between
#     waves. Directly interpretable, but not corrected for chance.

reliability_one <- function(var, event, type) {
  a <- dplyr::filter(w1_sub, Event == event)[[var]]
  b <- dplyr::filter(wave2,  Event == event)[[var]]
  k <- !is.na(a) & !is.na(b)
  if (sum(k) < 10) return(NULL)
  a <- a[k]; b <- b[k]

  if (type == "continuous") {
    score <- irr::icc(cbind(a, b), model = "twoway",
                      type = "agreement", unit = "single")$value
    consistent <- abs(b - a) <= AGREE_TOL * pmax(abs(a), 1e-9)
  } else {
    score <- irrCAC::gwet.ac1.raw(cbind(a, b))$est$coeff.val
    consistent <- a == b
  }
  tibble::tibble(Variable = var, Event = event, Type = type,
                 N = sum(k), Score = score,
                 Pct_Agreement = 100 * mean(consistent))
}

reliability_results <- purrr::pmap_dfr(
  tidyr::expand_grid(
    var = c(CONTINUOUS, CATEGORICAL),
    event = names(EVENTS)) %>%
    dplyr::mutate(type = ifelse(var %in% CONTINUOUS, "continuous", "categorical")),
  function(var, event, type) reliability_one(var, event, type))

# --- Table 2: agreement by event and response type ---------------------------
table2 <- reliability_results %>%
  dplyr::mutate(Years = YRS_ELAPSED[Event]) %>%
  dplyr::group_by(Event, Years, Type) %>%
  dplyr::summarise(Items = dplyr::n(),
                   Agreement = round(mean(Pct_Agreement), 1),
                   Reliability = round(mean(Score), 2), .groups = "drop") %>%
  dplyr::arrange(Years)
print(table2)

# --- Figure 3: distribution of item reliability by event and response type ---
# The manuscript figure. Each box is the spread of item-level reliability
# within one event and response type; individual items are overlaid so the
# tail of weakly recalled items stays visible.
EVENT_ORDER <- c("Ondoy", "Ulysses", "Karina")   # oldest recall first

fig3 <- ggplot(reliability_results %>%
                 dplyr::mutate(Event = factor(Event, levels = EVENT_ORDER)),
               aes(Event, Score, fill = Event)) +
  geom_boxplot(outlier.shape = NA, alpha = .35, width = .6) +
  geom_jitter(aes(colour = Event), width = .12, height = 0,
              size = 1.8, alpha = .75, show.legend = FALSE) +
  facet_wrap(~ Type) +
  labs(x = NULL, y = "Reliability index (ICC(2,1) / Gwet's AC1)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
print(fig3)
# ggsave("Fig3_reliability.png", fig3, width = 7.48, height = 4.5, dpi = 600)

# --- Figure 3b (supplementary view): every item, named ------------------------
# Kept because it shows WHICH items sit in the lower tail, which the boxplot
# hides. The left margin is widened so long variable names are not clipped.
fig3b <- ggplot(reliability_results,
                aes(reorder(Variable, Score), Score, colour = Event)) +
  geom_point(size = 2.4, alpha = .85) +
  coord_flip() +
  facet_wrap(~ Type, scales = "free_y") +
  labs(x = NULL, y = "Reliability index (ICC(2,1) / Gwet's AC1)", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.margin = margin(5.5, 5.5, 5.5, 14))   # room for long item names
# ggsave("Fig3b_reliability_by_item.png", fig3b, width = 9, height = 8, dpi = 600)

## =============================================================================
## RQ2  Who recalls consistently? Event characteristics vs household attributes
## =============================================================================
# Household-level consistency is regressed on event characteristics (depth,
# recall distance) and household attributes, with event fixed effects and a
# household random intercept. The question is whether any household
# characteristic survives once the event is accounted for.

consistency_long <- purrr::map_dfr(CONTINUOUS, function(v) {
  purrr::map_dfr(names(EVENTS), function(ev) {
    a <- dplyr::filter(w1_sub, Event == ev)
    b <- dplyr::filter(wave2,  Event == ev)
    tibble::tibble(
      HHID = a$HHID, Event = ev, Variable = v,
      Consistent = as.integer(abs(b[[v]] - a[[v]]) <=
                              AGREE_TOL * pmax(abs(a[[v]]), 1e-9)),
      Depth = a$Depth, Years = YRS_ELAPSED[ev],
      Head_Age = a$Head_Age, Family_Size = a$Family_Size,
      Income_Classification = a$Income_Classification,
      HH_Class_Education = a$HH_Class_Education)
  })
})

m_rq2 <- lme4::glmer(
  Consistent ~ scale(Depth) + scale(Years) + scale(Head_Age) +
               scale(Family_Size) + Income_Classification +
               HH_Class_Education + (1 | HHID) + (1 | Variable),
  data = consistency_long, family = binomial,
  control = lme4::glmerControl(optimizer = "bobyqa"))

# Fixed effects. summary()$coefficients is used rather than broom::tidy(),
# which has no method for merMod objects.
table5 <- as.data.frame(summary(m_rq2)$coefficients) %>%
  tibble::rownames_to_column("term") %>%
  setNames(c("term", "estimate", "std.error", "statistic", "p.value")) %>%
  dplyr::mutate(OR = exp(estimate),
                dplyr::across(where(is.numeric), ~ round(.x, 3)))
print(table5)

# Random effects. These answer RQ2 directly: how much of the variation in
# response consistency sits between households, and how much between items.
vc_rq2 <- as.data.frame(lme4::VarCorr(m_rq2))
print(vc_rq2[, c("grp", "vcov", "sdcor")])

## =============================================================================
## RQ3  Does measured reliability propagate into flood damage models?
## =============================================================================
# An FDM family is enumerated exhaustively: flood depth is forced into every
# specification (it is the primary predictor in the FDM literature) and all
# combinations of up to MAX_EXTRA further predictors are added. Each
# specification is fitted twice -- once on wave-1 responses, once on wave-2 --
# and the two fits compared.

POOL      <- c("FloodDuration", "House_Classification", "Head_Age",
               "Family_Size", "N_Income_Earners", "Income_Classification",
               "Floors", "Plinth_Height", "FirstFloor_Height")
MAX_EXTRA <- 5

specs <- unlist(lapply(0:MAX_EXTRA, function(k)
  if (k == 0) list(character(0)) else
    combn(POOL, k, simplify = FALSE)), recursive = FALSE)
cat("Specifications enumerated:", length(specs), "\n")

fit_spec <- function(vars, dat) {
  f <- as.formula(paste("HDR ~ sqrt(Depth) + 0",
                        if (length(vars)) paste("+", paste(vars, collapse = " + ")) else ""))
  m <- try(lm(f, data = dat), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  p <- predict(m, dat); r <- dat$HDR - p
  list(coef = coef(m),
       r2   = 1 - sum(r^2) / sum((dat$HDR - mean(dat$HDR))^2),
       rmse = sqrt(mean(r^2)), mae = mean(abs(r)), mbe = mean(r))
}

# Mean reliability of the variables entering each specification
rel_lookup <- reliability_results %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(Score = mean(Score), .groups = "drop") %>%
  tibble::deframe()

compare_specs <- function(group_events) {
  d1 <- dplyr::filter(w1_sub, Event %in% group_events)
  d2 <- dplyr::filter(wave2,  Event %in% group_events)
  purrr::map_dfr(seq_along(specs), function(i) {
    v  <- specs[[i]]
    f1 <- fit_spec(v, d1); f2 <- fit_spec(v, d2)
    if (is.null(f1) || is.null(f2)) return(NULL)
    tibble::tibble(
      Spec = i, N_Variables = length(v) + 1,
      Mean_Reliability = mean(rel_lookup[c("Depth", v)], na.rm = TRUE),
      Abs_Delta_R2   = abs(f2$r2   - f1$r2),
      Abs_Delta_RMSE = abs(f2$rmse - f1$rmse),
      Abs_Delta_MAE  = abs(f2$mae  - f1$mae),
      Delta_MBE      = f2$mbe - f1$mbe,
      Pct_Delta_Depth = 100 * (f2$coef[["sqrt(Depth)"]] - f1$coef[["sqrt(Depth)"]]) /
                               f1$coef[["sqrt(Depth)"]])
  })
}

fam_ondoy <- compare_specs("Ondoy")       %>% dplyr::mutate(Group = "Ondoy")
fam_post  <- compare_specs(c("Ulysses", "Karina")) %>% dplyr::mutate(Group = "Post-Ondoy")
family    <- dplyr::bind_rows(fam_ondoy, fam_post)

# --- Coefficient stability ---------------------------------------------------
cat("\nMedian % change in the depth coefficient, by group:\n")
print(family %>% dplyr::group_by(Group) %>%
        dplyr::summarise(Median_Pct_Delta_Depth = round(median(Pct_Delta_Depth), 2)))

# --- Figure 8: instability by model size -------------------------------------
fig8 <- family %>%
  tidyr::pivot_longer(c(Abs_Delta_R2, Abs_Delta_RMSE, Abs_Delta_MAE),
                      names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(factor(N_Variables), Value)) +
  geom_boxplot(fill = "grey95", colour = "grey30", outlier.size = 1) +
  facet_grid(Metric ~ Group, scales = "free_y") +
  labs(x = "Number of variables in model", y = NULL)

# --- Figure 9: reliability vs reproducibility --------------------------------
rho <- family %>% dplyr::group_by(Group) %>%
  dplyr::summarise(rho = round(cor(Mean_Reliability, Abs_Delta_RMSE,
                                   method = "spearman"), 2))
print(rho)

fig9 <- ggplot(family, aes(Mean_Reliability, Abs_Delta_RMSE, colour = Group)) +
  geom_point(alpha = .25, size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mean reliability of model variables", y = "|ΔRMSE|", colour = NULL)

# --- Figure 4: bivariate damage curves ---------------------------------------
curve_df <- purrr::map_dfr(names(EVENTS), function(ev) {
  d1 <- dplyr::filter(w1_sub, Event == ev); d2 <- dplyr::filter(wave2, Event == ev)
  b1 <- coef(lm(HDR ~ sqrt(Depth) + 0, d1)); b2 <- coef(lm(HDR ~ sqrt(Depth) + 0, d2))
  grid <- seq(0, 6, length.out = 200)
  dplyr::bind_rows(
    tibble::tibble(Event = ev, Wave = "Wave 1", Depth = grid, HDR = b1 * sqrt(grid)),
    tibble::tibble(Event = ev, Wave = "Wave 2", Depth = grid, HDR = b2 * sqrt(grid)))
})
fig4 <- ggplot(curve_df, aes(Depth, HDR, colour = Event, linetype = Wave)) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Flood depth (m)", y = "House damage rate (HDR)")

## ---- SESSION INFO -----------------------------------------------------------
sessionInfo()
