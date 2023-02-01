# Parametrize a distribution for health status changes
# NHP model - demographic module (942)
# author Paul Seamer


# README ----
# An elicitation workshop on population health status took place in October 2022.
# Here, we take the output from the workshop (a single distribution) and convert
# it to input values for the model.
# The workshop distribution is a gamma distribution defined in the range [.44, Inf],
# note this is different from a truncated distribution.


# in this script ----
# 1 read data
# 2 QOI distribution from workshop
# 3 baseline QOI
# 4 shifted QOI distributions
# 5 model input
# 6 LE variants
# 7 model input gamma distribution
# 8 convert adjustment to years


# packages ----
library("tidyverse")
library("here")
library("MetBrewer")
library("patchwork")
library("rriskDistributions")


# parameters ----
startyr <- 2018
endyr <- 2035
gam_n <- 1e5
gam_ll <- .44
gam_shp <- 2.36
gam_rt <- 50.6

# palette
# met.brewer("VanGogh2")
pal <- MetPalettes$VanGogh2[[1]]




# 1 read data ----
# historic disability free life expectancy (sourced from Chris White @ ONS)
# projected period life expectancy (sourced from NPP 2018b)
le_dat <- read_csv(here("_raw_data", "longrun_ts_le_65.csv"))
dfle_dat <- read_csv(here("_raw_data", "longrun_ts_dfle_65.csv"))
ex_dat <- readRDS(here("data", "ex_2018b_dat.rds"))

# we use period over cohort LE
ex_dat <- ex_dat |> filter(type == "period")




# 2 QOI distribution from workshop ----
# for a typical person of age 65 years in 2035, what proportion of their
# remaining life expectancy will be spent free of disability (i.e. without a
# limiting long-standing illness)? range [0, 100]
set.seed(42)
qoi_dist <- gam_ll + rgamma(n = gam_n, shape = gam_shp, rate = gam_rt)
qoi_dist <- tibble(qoi = qoi_dist)

# quantile(qoi_dist$qoi, c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1))

p1 <- ggplot(data = qoi_dist) +
  geom_histogram(aes(x = qoi), binwidth = .01, fill = pal[3], color = "#ffffff") +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(2, "mm"),
    axis.ticks.x = element_line(linewidth = .5, color = "#ebebeb"),
    legend.position = c(.8, .68)
  )

ggsave(here("figures", "qoi_wshop_dist_histogram.png"), p1, width = 144, height = 100, units = c("mm"))




# 3 baseline QOI ----
# for a typical female/male of age 65 years in 2018, what proportion of their
# remaining life expectancy will be spent free of disability (i.e. without a
# limiting long-standing illness)?
base_dfle <- dfle_dat |>
  filter(start == startyr) |>
  mutate(var = "dfle")
base_le <- le_dat |>
  filter(start == startyr) |>
  mutate(var = "le")

base_dat <- bind_rows(base_dfle, base_le) |>
  select(-source) |>
  pivot_longer(m:f, names_to = "sex", values_to = "val") |>
  pivot_wider(names_from = var, values_from = val) |>
  mutate(dle = le - dfle, dfle_p = dfle / le * 100)

# derive multiplicative constant to shift workshop distribution for females/males
mx_const <- base_dat |>
  mutate(mx = dfle_p / mean(dfle_p)) |>
  select(sex, mx, dfle_p)




# 4 shifted QOI distributions ----
shift_dist <- function(ll = gam_ll,
                       n = gam_n,
                       shp = gam_shp,
                       rt = gam_rt,
                       mx_const_df = mx_const,
                       sex = NULL) {
  if (is.null(sex)) {
    ll + rgamma(n = n, shape = shp, rate = rt)
  } else {
    (ll + rgamma(n = n, shape = shp, rate = rt)) * mx_const_df |>
      filter(sex == {{ sex }}) |>
      pull(mx)
  }
}

set.seed(42)
wshop_dist <- shift_dist(sex = NULL)
set.seed(42)
f_dist <- shift_dist(sex = "f")
set.seed(42)
m_dist <- shift_dist(sex = "m")

# quantile(wshop_dist, c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1))
# quantile(f_dist, c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1))
# quantile(m_dist, c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1))

qoi_dists <- tibble(
  p = wshop_dist,
  f = f_dist,
  m = m_dist
) |>
  pivot_longer(cols = everything(), names_to = "sex", values_to = "qoi")

# for plotting
const_f <- mx_const |>
  filter(sex == "f") |>
  pull(mx)
const_m <- mx_const |>
  filter(sex == "m") |>
  pull(mx)
unwt_mn_dfle_p <- mx_const |>
  summarise(mn = mean(dfle_p)) |>
  pull(mn)

p2 <- ggplot(data = qoi_dists) +
  geom_histogram(
    aes(
      x = qoi,
      group = sex,
      fill = sex,
    ),
    color = "#ffffff",
    alpha = .4,
    stat = "bin",
    position = "identity",
    binwidth = .01
  ) +
  geom_vline(aes(xintercept = (const_f * unwt_mn_dfle_p) / 100), linewidth = .5, color = pal[1]) +
  geom_vline(aes(xintercept = (const_m * unwt_mn_dfle_p) / 100), linewidth = .5, color = pal[5]) +
  geom_vline(aes(xintercept = unwt_mn_dfle_p / 100), linewidth = .5, color = pal[8]) +
  scale_fill_manual(values = pal[c(1, 5, 8)]) +
  scale_alpha_manual(values = rep(.5, 3)) +
  scale_x_continuous(name = NULL, breaks = seq(.4, .8, .1)) +
  scale_y_continuous(name = NULL, expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(2, "mm"),
    axis.ticks.x = element_line(linewidth = .5, color = "#ebebeb"),
    legend.position = c(.8, .68)
  )

p3 <- ggplot(data = qoi_dists) +
  geom_density(
    aes(
      x = qoi,
      group = sex,
      fill = sex,
    ),
    stat = "density",
    color = "#ffffff",
    alpha = .4
  ) +
  geom_vline(aes(xintercept = (const_f * unwt_mn_dfle_p) / 100), linewidth = .5, color = pal[1]) +
  geom_vline(aes(xintercept = (const_m * unwt_mn_dfle_p) / 100), linewidth = .5, color = pal[5]) +
  geom_vline(aes(xintercept = unwt_mn_dfle_p / 100), linewidth = .5, color = pal[8]) +
  scale_fill_manual(values = pal[c(1, 5, 8)]) +
  scale_alpha_manual(values = rep(.5, 3)) +
  scale_x_continuous(name = NULL, breaks = seq(.4, .8, .1)) +
  scale_y_continuous(name = NULL, expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(2, "mm"),
    axis.ticks.x = element_line(linewidth = .5, color = "#ebebeb"),
    legend.position = c(.8, .68)
  )

ggsave(here("figures", "qoi_shifted_dists_histogram.png"), p2, width = 144, height = 100, units = c("mm"))
ggsave(here("figures", "qoi_shifted_dists_density.png"), p3, width = 144, height = 100, units = c("mm"))




# 5 model input ----
# separately for females/males (and by LE variant), the amount by which a record
# in the model will have its age adjusted to reflect changes in age-specific health status,
# expressed as a percentage of the projected increase in life expectancy
# (for a given single year of age) range [-Inf, Inf]
ex_dat_chg <- ex_dat |>
  filter(year %in% as.character(c(startyr, endyr)), age == 65) |>
  pivot_wider(names_from = year, values_from = ex, names_prefix = "year_") |>
  mutate(chg = .data[[str_c("year_", endyr)]] - .data[[str_c("year_", startyr)]])

hs_input <- function(var = "ple",
                     base_dfle_df = base_dfle,
                     mx_const_df = mx_const,
                     ex_dat_df = ex_dat_chg,
                     sex = NULL) {
  if (!sex %in% c("f", "m")) {
    stop(paste0("sex arg must be supplied as 'f' or 'm'"))
  }
  mx <- mx_const_df |>
    filter(sex == {{ sex }}) |>
    pull(mx)
  ex_endyr <- ex_dat_df |>
    filter(sex == {{ sex }}, var == {{ var }}) |>
    pull(year_2035)
  proj_chg <- ex_dat_df |>
    filter(sex == {{ sex }}, var == {{ var }}) |>
    pull(chg)
  dfle_start <- base_dfle_df |> pull(sex)

  dfle_end <- shift_dist(sex = sex) * ex_endyr
  dfle_chg <- dfle_end - dfle_start
  dfle_chg / proj_chg # this is the input value
}

# females
set.seed(42)
quant_f <- quantile(
  hs_input(var = "ple", sex = "f"),
  c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1)
)

# males
set.seed(42)
quant_m <- quantile(
  hs_input(var = "ple", sex = "m"),
  c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1)
)

set.seed(42)
plot_dat <- tibble(
  f = hs_input(var = "ple", sex = "f"),
  m = hs_input(var = "ple", sex = "m")
) |>
  pivot_longer(everything(), names_to = "sex", values_to = "pct")

p4 <- ggplot(data = plot_dat) +
  geom_histogram(
    aes(x = pct, group = sex, fill = sex),
    stat = "bin",
    position = "identity",
    binwidth = .1,
    color = "#ffffff",
    alpha = .4
  ) +
  geom_vline(aes(xintercept = 0), linewidth = .5, color = "#2c2825") +
  annotate("text", x = -1, y = 8000, label = "aged", color = "#2c2825") +
  annotate("text", x = 1, y = 8000, label = "de-aged", color = "#2c2825") +
  scale_fill_manual(values = pal[c(1, 5)]) +
  scale_alpha_manual(values = rep(.5, 3)) +
  scale_x_continuous(name = NULL, breaks = -1:5) +
  scale_y_continuous(name = NULL, expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(2, "mm"),
    axis.ticks.x = element_line(linewidth = .5, color = "#ebebeb"),
    legend.position = c(.8, .8)
  )

p5 <- ggplot(data = plot_dat) +
  geom_density(
    aes(
      x = pct,
      group = sex,
      fill = sex,
    ),
    stat = "density",
    color = "#ffffff",
    alpha = .4
  ) +
  geom_vline(aes(xintercept = 0), linewidth = .5, color = "#2c2825") +
  annotate("text", x = -1, y = .8, label = "aged", color = "#2c2825") +
  annotate("text", x = 1, y = .8, label = "de-aged", color = "#2c2825") +
  scale_fill_manual(values = pal[c(1, 5)]) +
  scale_alpha_manual(values = rep(.5, 3)) +
  scale_x_continuous(name = NULL, breaks = -1:5) +
  scale_y_continuous(name = NULL, expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(2, "mm"),
    axis.ticks.x = element_line(linewidth = .5, color = "#ebebeb"),
    legend.position = c(.8, .8)
  )

ggsave(here("figures", "hs_input_dists_histogram.png"), p4, width = 144, height = 100, units = c("mm"))
ggsave(here("figures", "hs_input_dists_density.png"), p5, width = 144, height = 100, units = c("mm"))


# model input quantiles
hs_input_qts <- tibble(
  qt = labels(quant_f),
  q_f = quant_f,
  q_m = quant_m
)

# hs_input_qts




# 6 LE variants ----
# the model input value depends on the expected change in LE. ONS publish three LE variants (for England)
# these map to the population projection variants i.e. some population projection variants assume
# a greater/lesser increase in LE
# what effect do alternative LE variants have on the model input distribution?
args <- as.list(expand_grid(var = c("lle", "ple", "hle"), sex = c("f", "m")))
hs_input_levars <- purrr::pmap(args, hs_input)

names(hs_input_levars) <- paste(args$var, args$sex, sep = "_")
hs_input_levars_df <- enframe(hs_input_levars, name = "var", value = "val") |>
  unnest(cols = c(val)) |>
  mutate(sex = str_extract(var, ".$"), var = str_remove(var, "_(f|m)$"))

sex_label <- c(f = "women", m = "men")

p6 <- ggplot(data = hs_input_levars_df) +
  geom_violin(aes(x = val, y = var, fill = var, color = var), show.legend = FALSE) +
  facet_wrap(vars(sex), labeller = labeller(sex = sex_label)) +
  geom_vline(aes(xintercept = 0), linewidth = .5, color = "#2c2825") +
  scale_fill_manual(values = pal[1:3]) +
  scale_color_manual(values = pal[1:3]) +
  scale_x_continuous(name = NULL, breaks = seq(-4, 10, 2)) +
  scale_y_discrete(name = NULL) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
  )

ggsave(here("figures", "hs_input_dists_by_levar.png"), p6, width = 144, height = 100, units = c("mm"))




# 7 model input gamma distribution ----
# can we fit a new gamma distribution to the model input distribution and obtain its parameters
# such that we can feed the parameters to a random gamma generator and generate model input values
# directly?
# using package rriskDistributions
p <- c(00, .01, .05, .1, .25, 0.5, .75, .9, .95, .99)

# example is for females and principal LE variant
q <- quantile(
  hs_input(var = "ple", sex = "f"),
  probs = p
)
# gamma distribution is always positive therefore a shift constant is required
shift_const <- q[1]
shift_q <- q - shift_const

fit_gamma <- rriskDistributions::get.gamma.par(
  p = p,
  q = shift_q,
  show.output = FALSE,
  plot = TRUE,
  tol = 0.001,
  fit.weights = rep(1, length(p)),
  scaleX = c(0.1, 0.9)
)

test_gamma <- rgamma(n = gam_n, shape = fit_gamma[1], rate = fit_gamma[2])

test_fit <- tibble(
  hs_input_fx = q,
  fit_gamma = quantile(test_gamma, probs = p) + shift_const
) |>
  mutate(diff = hs_input_fx - fit_gamma)

# test_fit
# ergo for females + principal LE variant we could use rgamma(shape = 2.317666, rate = 2.862401)
# to generate a distribution of model input values




# 8 convert adjustment to years ----
# example for persons of age 75 years
ex_chg_75 <- ex_dat |>
  filter(year %in% c(startyr, endyr), age == 75) |>
  pivot_wider(names_from = year, values_from = ex, names_prefix = "year_") |>
  mutate(ex_chg = year_2035 - year_2018, var = paste(var, sex, sep = "_")) |>
  select(var, ex_chg)

deage_yrs_75 <- enframe(hs_input_levars, name = "var", value = "val") |>
  left_join(ex_chg_75, by = "var") |>
  mutate(deage_yrs = map2(val, ex_chg, `*`)) |>
  mutate(sex = str_extract(var, ".$"), var = str_remove(var, "_(f|m)$")) |>
  select(var, sex, deage_yrs) |>
  unnest(c(deage_yrs))

p7 <- ggplot(data = deage_yrs_75) +
  geom_violin(aes(x = deage_yrs, y = var, fill = var, color = var), show.legend = FALSE) +
  facet_wrap(vars(sex), labeller = labeller(sex = sex_label)) +
  geom_vline(aes(xintercept = 0), linewidth = .5, color = "#2c2825") +
  scale_fill_manual(values = pal[1:3]) +
  scale_color_manual(values = pal[1:3]) +
  scale_x_continuous(name = NULL, breaks = seq(-4, 10, 2)) +
  scale_y_discrete(name = NULL) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
  )

ggsave(here("figures", "deage_yrs_dists_by_levar.png"), p7, width = 144, height = 100, units = c("mm"))

