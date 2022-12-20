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


# packages ----
library("tidyverse")
library("here")
library("MetBrewer")
library("patchwork")


# parameters ----
startyr <- 2018
endyr   <- 2035
gam_n   <- 1e5
gam_ll  <- .44
gam_shp <- 2.36
gam_rt  <- 50.6

# palette
# met.brewer("VanGogh2")
pal <- MetPalettes$VanGogh2[[1]]




# read data ----
# historic disability free life expectancy (sourced from Chris White @ ONS)
# projected period life expectancy (sourced from NPP 2018b)
le_dat   <- read_csv(here("_raw_data", "longrun_ts_le_65.csv"))
dfle_dat <- read_csv(here("_raw_data", "longrun_ts_dfle_65.csv"))
ex_dat   <- readRDS(here("data", "ex_2018b_dat.rds"))




# QOI distribution from workshop ----
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
    axis.ticks.length =  unit(2, "mm"),
    axis.ticks.x = element_line(linewidth = .5, color = "grey92"),  
    legend.position = c(.8, .68)
  )

ggsave(here("figures", "qoi_wshop_dist_histogram.png"), p1, width = 144, height = 100, units = c("mm"))




# baseline QOI ----
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
unwt_mn_dfle_p <- mean(base_dat$dfle_p)
const_f <- filter(base_dat, sex == "f")$dfle_p / unwt_mn_dfle_p
const_m <- filter(base_dat, sex == "m")$dfle_p / unwt_mn_dfle_p




# shifted QOI distributions ----
shift_dist <- function(ll = gam_ll,
                       n = gam_n,
                       shp = gam_shp,
                       rt = gam_rt,
                       mx_const = NULL) {
  if (is.null(mx_const)) {
    ll + rgamma(n = n, shape = shp, rate = rt)
  } else {
    (ll + rgamma(n = n, shape = shp, rate = rt)) * mx_const
  }
}

set.seed(42)
wshop_dist <- shift_dist(mx_const = NULL)
set.seed(42)
f_dist     <- shift_dist(mx_const = const_f)
set.seed(42)
m_dist     <- shift_dist(mx_const = const_m)

# quantile(wshop_dist, c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1))
# quantile(f_dist, c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1))
# quantile(m_dist, c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1))

qoi_dists <- tibble(
  p = wshop_dist,
  f = f_dist,
  m = m_dist
  ) |> 
  pivot_longer(cols = everything(), names_to = "sex", values_to = "qoi")

p2 <- ggplot(data = qoi_dists) +
  geom_histogram(
    aes(x = qoi,
        group = sex,
        fill = sex,
        ),
    color = "#ffffff",
    alpha = .4,
    stat = "bin",
    position = "identity",
    binwidth = .01) +
  geom_vline(aes(xintercept = (const_f * unwt_mn_dfle_p) / 100), linewidth = .5, color = pal[1]) +
  geom_vline(aes(xintercept = (const_m * unwt_mn_dfle_p) / 100), linewidth = .5, color = pal[5]) +
  geom_vline(aes(xintercept = unwt_mn_dfle_p / 100), linewidth = .5, color = pal[8]) +
  scale_fill_manual(values = pal[c(1,5,8)]) +
  scale_alpha_manual(values = rep(.5, 3)) +
  scale_x_continuous(name = NULL, breaks = seq(.4, .8, .1)) +
  scale_y_continuous(name = NULL, expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length =  unit(2, "mm"),
    axis.ticks.x = element_line(linewidth = .5, color = "grey92"),  
    legend.position = c(.8, .68)
  )

p3 <- ggplot(data = qoi_dists) +
  geom_density(
    aes(x = qoi,
        group = sex,
        fill = sex,
        ),
    stat = "density",
    color = "#ffffff",
    alpha = .4) +
  geom_vline(aes(xintercept = (const_f * unwt_mn_dfle_p) / 100), linewidth = .5, color = pal[1]) +
  geom_vline(aes(xintercept = (const_m * unwt_mn_dfle_p) / 100), linewidth = .5, color = pal[5]) +
  geom_vline(aes(xintercept = unwt_mn_dfle_p / 100), linewidth = .5, color = pal[8]) +
  scale_fill_manual(values = pal[c(1,5,8)]) +
  scale_alpha_manual(values = rep(.5, 3)) +
  scale_x_continuous(name = NULL, breaks = seq(.4, .8, .1)) +
  scale_y_continuous(name = NULL, expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length =  unit(2, "mm"),
    axis.ticks.x = element_line(linewidth = .5, color = "grey92"),  
    legend.position = c(.8, .68)
  )

ggsave(here("figures", "qoi_shifted_dists_histogram.png"), p2, width = 144, height = 100, units = c("mm"))
ggsave(here("figures", "qoi_shifted_dists_density.png"), p3, width = 144, height = 100, units = c("mm"))




# model input ----
# separately for females/males, the amount by which a record in the model will
# have its age adjusted to reflect changes in age-specific health status,
# expressed as a percentage of the projected increase in life expectancy
# (for a given single year of age) range [-Inf, Inf]
dfle_start_f <- dfle_dat |> 
  filter(start == startyr) |> 
  pull(f)
dfle_start_m <- dfle_dat |> 
  filter(start == startyr) |> 
  pull(m)

ex_dat_consts <- ex_dat |>
  filter(type == "period", year %in% as.character(c(startyr, endyr)), age == 65) |> 
  pivot_wider(names_from = year, values_from = ex, names_prefix = "year_") |> 
  mutate(chg = .data[[str_c("year_", endyr)]] - .data[[str_c("year_", startyr)]])

ex_endyr_f  <- ex_dat_consts$year_2035[ex_dat_consts$sex == "f"]
ex_endyr_m  <- ex_dat_consts$year_2035[ex_dat_consts$sex == "m"]
proj_chg_f  <- ex_dat_consts$chg[ex_dat_consts$sex == "f"]
proj_chg_m  <- ex_dat_consts$chg[ex_dat_consts$sex == "m"]

hs_input <- function(dfle_start = NULL, ex_endyr = NULL, proj_chg = NULL, mx_const = NULL) {
  dfle_end <- shift_dist(mx_const = mx_const) * ex_endyr
  dfle_chg <- dfle_end - dfle_start
  adj <- dfle_chg / proj_chg
}

# females
set.seed(42)
quant_f <- quantile(
  hs_input(dfle_start = dfle_start_f, ex_endyr = ex_endyr_f, proj_chg = proj_chg_f, mx_const = const_f),
  c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1)
  )

# males
set.seed(42)
quant_m <- quantile(
  hs_input(dfle_start = dfle_start_m, ex_endyr = ex_endyr_m, proj_chg = proj_chg_m, mx_const = const_m),
  c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1)
)

set.seed(42)
plot_dat <- tibble(
  f = hs_input(dfle_start = dfle_start_f, ex_endyr = ex_endyr_f, proj_chg = proj_chg_f, mx_const = const_f),
  m = hs_input(dfle_start = dfle_start_m, ex_endyr = ex_endyr_m, proj_chg = proj_chg_m, mx_const = const_m)
  ) |> 
  pivot_longer(everything(), names_to = "sex", values_to = "pct")

p4 <- ggplot(data = plot_dat) +
  geom_histogram(
    aes(x = pct, group = sex, fill = sex),
    stat = "bin",
    position = "identity",
    binwidth = .1,
    color = "#ffffff",
    alpha = .4) +
  geom_vline(aes(xintercept = 0), linewidth = .5, color = "#2c2825") +
  annotate("text", x = -1, y = 8000, label = "aged", color =  "#2c2825") +
  annotate("text", x = 1, y = 8000, label = "de-aged", color =  "#2c2825") +
  scale_fill_manual(values = pal[c(1,5)]) +
  scale_alpha_manual(values = rep(.5, 3)) +
  scale_x_continuous(name = NULL, breaks = -1:5) +
  scale_y_continuous(name = NULL, expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length =  unit(2, "mm"),
    axis.ticks.x = element_line(linewidth = .5, color = "grey92"),  
    legend.position = c(.8, .8)
  )

p5 <- ggplot(data = plot_dat) +
  geom_density(
    aes(x = pct,
        group = sex,
        fill = sex,
    ),
    stat = "density",
    color = "#ffffff",
    alpha = .4) +
  geom_vline(aes(xintercept = 0), linewidth = .5, color = "#2c2825") +
  annotate("text", x = -1, y = .8, label = "aged", color =  "#2c2825") +
  annotate("text", x = 1, y = .8, label = "de-aged", color =  "#2c2825") +
  scale_fill_manual(values = pal[c(1,5)]) +
  scale_alpha_manual(values = rep(.5, 3)) +
  scale_x_continuous(name = NULL, breaks = -1:5) +
  scale_y_continuous(name = NULL, expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length =  unit(2, "mm"),
    axis.ticks.x = element_line(linewidth = .5, color = "grey92"),  
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
