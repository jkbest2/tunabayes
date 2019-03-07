library(rstan)
library(bayesplot)
library(tidyverse)
library(gridExtra)
library(ggridges)
library(ggsci)
library(viridis)

## Get data
source("src/01_setup.R")
## Get ggplot theme
source("src/02_fitfns.R")

##-Figure 1: CPUE and catch series plots----------------------------------------
cpue_catch <- tibble(year = 1967:1989,
                     catch = tuna_data$C,
                     index = tuna_data$I)
catch_plot <- ggplot(cpue_catch, aes(x = year, y = catch)) +
  geom_line(size = 1) +
  labs(# title = "South Atlantic albacore catch",
       x = "Year", y = "Catch biomass (Gg)") +
  theme_jkb(10)
cpue_plot <- ggplot(cpue_catch, aes(x = year, y = index)) +
  geom_line(size = 1) +
  labs(# title = "South Atlantic albacore catch per unit effort",
       x = "Year", y = "CPUE (kg per 100 hooks)") +
  theme_jkb(10)
cc_plot <- grid.arrange(catch_plot, cpue_plot, nrow = 2L)
ggsave("figs/fig1_catch_cpue.tiff", cc_plot, width = 6, height = 4)
ggsave("figs/fig1_catch_cpue.pdf", cc_plot, device = cairo_pdf,
       width = 6, height = 4)

##-Model fits-------------------------------------------------------------------

fit_datetime <- "2018-12-11 22:42:05"
load(paste0("results/", fit_datetime, "_fits.Rdata"))

## Define colors for parameterizations
param_levels <- c("Centered", "Truncated",
                  "Constrained P", "Noncentered",
                  "Marginal q", "Explicit F",
                  "Explicit F marg q")
wong_colors <- c(rgb(0, 0, 0, max = 255),       # Black
                 rgb(230, 159, 0, max = 255),   # Orange
                 rgb(86, 180, 233, max = 255),  # Sky blue
                 rgb(0, 158, 115, max = 255),   # Bluish green
                 rgb(240, 228, 66, max = 255),  # Yellow
                 rgb(0, 114, 178, max = 255),   # Blue
                 rgb(213, 94, 0, max = 255),    # Vermillion
                 rgb(204, 121, 167, max = 255)) # Reddish purple
param_colors <- wong_colors[2:8]
## param_colors <- pal_futurama()(7)
names(param_colors) <- param_levels

post_df <- fit_df %>%
  transmute(model_name = model_name,
            post = map(fit, rstan::extract, inc_warmup = FALSE))

diag_df <- fit_df %>%
  mutate(ess = map(fit, ~ summary(.)$summary[, "n_eff"])) %>%
  transmute(model_name = factor(model_name, param_levels),
            adj = adj,
            td_total = map_dbl(fit, get_num_max_treedepth),
            div_total = map_dbl(fit, get_num_divergent),
            ## na.rm to get rid of fixed (Pmed[1]) pars with NaN ESS
            exp_ess = map_dbl(ess, mean, na.rm = TRUE),
            min_ess = map_dbl(ess, min, na.rm = TRUE),
            min_ess_par = map_chr(ess,
                                  function(p) attr(p, "names")[which.min(p)]),
            max_ess = map_dbl(ess, max, na.rm = TRUE))

##-Table 2: sampler diagnostics-------------------------------------------------
## Generate a table for comparing diagnostics between default settings and
## adjusted settings.
default_diag <- diag_df %>%
  filter(!adj) %>%
  select(model_name,
         `Exc Treedepth` = td_total,
         `Divergences` = div_total)
adj_diag <- diag_df %>%
  filter(adj) %>%
  select(model_name = model_name,
         `Exc Treedepth Adj` = td_total,
         `Divergences Adj` = div_total)
pdiag <- left_join(default_diag, adj_diag, by = "model_name") %>%
  select(` ` = model_name,
         `Divergences`, `Divergences Adj`,
         `Exc Treedepth`, `Exc Treedepth Adj`)
knitr::kable(pdiag)

##-Table 3: effective sample sizes----------------------------------------------
ess_df <- diag_df %>%
  select(model_name, adj, min_ess) %>%
  spread(adj, min_ess) %>%
  rename(` ` = model_name,
         Default = `FALSE`,
         Adjusted = `TRUE`)
knitr::kable(ess_df, digits = 0)

##-Table NOT USED: diagnostic changes with adjustments
## Not sure if this will be helpful. May consider a plot where all counts are
## positive and arrows go toward zero. A table may also be a better option here.
## diag_arrow <- arrow(length = unit(0.5, "cm"))
## pdiag %>%
##   select(model_name = ` `,
##          div = Divergences,
##          div_adj = `Divergences Adj`,
##          td = `Exc Treedepth`,
##          td_adj = `Exc Treedepth Adj`) %>%
##   mutate(model_name = factor(model_name, levels = rev(model_name)),
##          div = div,
##          div_adj = div_adj) %>%
##   ggplot(aes(y = model_name, yend = model_name)) +
##   geom_segment(aes(x = div, xend = div_adj),
##                color = hcl(15, 100, 65), size = 2,
##                arrow = diag_arrow) +
##   geom_segment(aes(x = td, xend = td_adj),
##                color = hcl(195, 100, 65), size = 2,
##                arrow = diag_arrow) +
##   geom_vline(xintercept = 0, size = 1,
##              linetype = "dashed", color = rgb(0, 0, 0, 0.05)) +
##   ## coord_cartesian(xlim = c(0, 20)) +
##   labs(title = "NUTS diagnostics before and after adjusting sampler parameters",
##        x = "Number of divergent samples or samples that exceed max treedepth") +
##   theme_jkb(base_size = 9) +
##   theme(axis.title.y = element_blank())

## Generate table of timing measures (ESS, ESS per time)
time_df <- fit_df %>%
  mutate(model_name = factor(model_name, param_levels)) %>%
  left_join(diag_df, by = c("model_name", "adj")) %>%
  mutate(times = map(fit, get_elapsed_time)) %>%
  transmute(model_name = model_name,
            adj = adj,
            warmup = map_dbl(times, ~ sum(.[, "warmup"])),
            sample = map_dbl(times, ~ sum(.[, "sample"])),
            min_ess_rate = min_ess / sample,
            exp_ess_rate = exp_ess / sample)

##-Figure 2: Plot of ESS rates with default and adjusted sampler settings------
## Mostly looks good. Need to figure out how to adjust margins to balance
## negative space more effectively.
time_df %>%
  ggplot(aes(x = adj, y = min_ess_rate,
             color = model_name, group = model_name)) +
  geom_path(size = 1) + geom_point(size = 1.5) +
  scale_y_log10(minor_breaks = c(seq(0.0, 1, 0.1),
                                 seq(1, 10, 1),
                                 seq(10, 100, 10))) +
  scale_x_discrete(labels = c("Default", "Adjusted")) +
  scale_color_manual(values = param_colors) +
  geom_text(aes(label = model_name),
            data = filter(time_df, adj),
            hjust = "left", nudge_x = 0.03,
            color = "black", family = "Montserrat", size = 2.5) +
  labs(# title = "MCMC sampler efficiency",
       x = "Sampler tuning parameters",
       y = "Effectively independent samples per second",
       color = "Model parameterization") +
  guides(color = FALSE)+
  theme_jkb(base_size = 9) +
  theme(panel.grid.major.y = element_line(color = rgb(0, 0, 0, 0.15)),
        panel.grid.minor.y = element_line(color = rgb(0, 0, 0, 0.05)))
ggsave("figs/sampler_efficiency.png", width = 6, height = 4)

## default_time <- time_df %>%
##   filter(!adj) %>%
##   select(model_name, sample, min_ess_rate)

## adj_time <- time_df %>%
##   filter(adj) %>%
##   select(model_name, sample, min_ess_rate)

##-Posterior densities - MSY----------------------------------------------------
## msy_post <- fit_df %>%
##   filter(adj) %>%
##   transmute(model_name = factor(model_name, param_levels),
##             MSY = map(fit, rstan::extract, pars = "MSY")) %>%
##   unnest() %>% unnest()

## mgt_summ <- fit_df %>%
##   filter(adj) %>%
##   transmute(model_name = factor(model_name, levels = rev(param_levels)),
##             val = map(fit, rstan::extract, pars = c("MSY", "FMSY", "Biomass[24]"))) %>%
##   unnest() %>%
##   mutate(par = rep(c("MSY", "FMSY", "Biomass 1990"), 7)) %>%
##   unnest() %>%
##   group_by(model_name, par) %>%
##   summarize(p025 = quantile(val, 0.025),
##             p10 = quantile(val, 0.1),
##             p25 = quantile(val, 0.25),
##             p50 = quantile(val, 0.5),
##             p75 = quantile(val, 0.75),
##             p90 = quantile(val, 0.9),
##             p975 = quantile(val, 0.975))

## ## Faceted plot of management quantiles; didn't want to mess with specifics for
## ## each facet, so used multiple plots below instead.
## mgt_summ %>%
##   ggplot(aes(y = model_name, yend = model_name, color = model_name)) +
##   geom_segment(aes(x = p025, xend = p975), size = 0.75) +
##   geom_segment(aes(x = p10, xend = p90), size = 1.125) +
##   geom_segment(aes(x = p25, xend = p75), size = 1.5) +
##   geom_point(aes(x = p50), size = 1.25) +
##   ## geom_density_ridges(aes(x = MSY), alpha = 0.2, data = msy_post, scale = 0.9) +
##   scale_x_continuous(minor_breaks = seq(0, 100, 2)) +
##   scale_color_manual(values = param_colors) +
##   ## coord_cartesian(xlim = c(5, 40)) +
##   facet_wrap(~ par, scales = "free_x") +
##   labs(title = "Posterior quantiles of MSY",
##        x = "MSY (Gg)") +
##   theme_jkb() +
##   theme(axis.title.y = element_blank(),
##         panel.grid.major.x = element_line(color = rgb(0, 0, 0, 0.15)),
##         panel.grid.minor.x = element_line(color = rgb(0, 0, 0, 0.05))) +
##   guides(color = FALSE)

## This density plot has a lot of overlap (which is kind of the point).
## Alternatives would be a "boxplot"-style plot, or the `ggridges` package to
## compare them vertically rather than via overlap.
## msy_post %>%
##   ggplot(aes(x = MSY, color = model_name, fill = model_name)) +
##   geom_density(alpha = 0.2, size = 0.5) +
##   scale_color_manual(values = param_colors) +
##   scale_fill_manual(values = param_colors) +
##   labs(title = "MSY Posteriors under different parameterizations",
##        x = "Maximum Sustainable Yield", y = "Density",
##        color = "Model", fill = "Model") +
##   coord_cartesian(xlim = c(0, 40)) +
##   theme_jkb(base_size = 10) +
##   theme(legend.position = c(0.9, 0.75))
## ggsave("figs/msy_post_dens.png", width = 6, height = 4)

##-Figure 3: Parameterization biomass time series-------------------------------
varyear <- paste0("Biomass[", 1:24, "]")
names(varyear) <- 1967:1990
biopost_summ <- fit_df %>%
  filter(adj) %>%
  transmute(model_name = factor(model_name, param_levels),
            B = map(fit, as.data.frame, pars = "Biomass")) %>%
  unnest() %>% unnest() %>%
  rename(!!varyear) %>%
  gather("year", "biomass", -model_name) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(model_name, year) %>%
  summarize(p10 = quantile(biomass, 0.1),
            p25 = quantile(biomass, 0.25),
            p50 = quantile(biomass, 0.5),
            p75 = quantile(biomass, 0.75),
            p90 = quantile(biomass, 0.9))

bp_dodge <- position_dodge(width = 0.9)
biopost_plot <- biopost_summ %>%
  ggplot(aes(x = year, y = p50,
             color = model_name, group = model_name)) +
  geom_vline(xintercept = 1989.5, linetype = "dashed",
             size = 0.5, color = rgb(0, 0, 0, 0.15)) +
  geom_linerange(aes(ymin = p10, ymax = p90),
               position = bp_dodge, size = 0.5) +
  geom_linerange(aes(ymin = p25, ymax = p75),
                 position = bp_dodge, size = 0.75) +
  geom_point(position = bp_dodge,
             size = 2.5, shape = "-", color = "black") +
  annotate(geom = "text", x = 1989.6, y = 0, hjust = 0,
           label = "Projected",
           family = "Montserrat", size = 2, color = rgb(0, 0, 0, 0.15)) +
  scale_color_manual(values = param_colors) +
  labs(# title = "Biomass posterior credible intervals",
       x = "Year", y = "Biomass (Gg)", color = "Parameterization") +
  coord_cartesian(ylim = c(0, 500)) +
  theme_jkb(base_size = 8) +
  theme(legend.position = c(0.9, 0.75))
ggsave("figs/fig2_biopost.tiff", biopost_plot, width = 6, height = 4)
ggsave("figs/fig2_biopost.pdf", biopost_plot, device = cairo_pdf,
       width = 6, height = 4)


##-Figure 4: Parameterization management posteriors-----------------------------
msy_summ <- fit_df %>%
  filter(adj) %>%
  transmute(model_name = factor(model_name, levels = rev(param_levels)),
            MSY = map(fit, rstan::extract, pars = "MSY")) %>%
  unnest() %>% unnest() %>%
  group_by(model_name) %>%
  summarize(p025 = quantile(MSY, 0.025),
            p10 = quantile(MSY, 0.1),
            p25 = quantile(MSY, 0.25),
            p50 = quantile(MSY, 0.5),
            p75 = quantile(MSY, 0.75),
            p90 = quantile(MSY, 0.9),
            p975 = quantile(MSY, 0.975))
fmsy_summ <- fit_df %>%
  filter(adj) %>%
  transmute(model_name = factor(model_name, levels = rev(param_levels)),
            FMSY = map(fit, rstan::extract, pars = "FMSY")) %>%
  unnest() %>% unnest() %>%
  group_by(model_name) %>%
  summarize(p025 = quantile(FMSY, 0.025),
            p10 = quantile(FMSY, 0.1),
            p25 = quantile(FMSY, 0.25),
            p50 = quantile(FMSY, 0.5),
            p75 = quantile(FMSY, 0.75),
            p90 = quantile(FMSY, 0.9),
            p975 = quantile(FMSY, 0.975))
Pfinal_summ <- fit_df %>%
  filter(adj) %>%
  transmute(model_name = factor(model_name, levels = rev(param_levels)),
            `Biomass 1990` = map(fit, rstan::extract, pars = "P_final")) %>%
  unnest() %>% unnest() %>%
  group_by(model_name) %>%
  summarize(p025 = quantile(`Biomass 1990`, 0.025),
            p10 = quantile(`Biomass 1990`, 0.1),
            p25 = quantile(`Biomass 1990`, 0.25),
            p50 = quantile(`Biomass 1990`, 0.5),
            p75 = quantile(`Biomass 1990`, 0.75),
            p90 = quantile(`Biomass 1990`, 0.9),
            p975 = quantile(`Biomass 1990`, 0.975))

##-Figure 4: Quantiles for comparison between parameterizations-----------------
msy_quant <- msy_summ %>%
  ggplot(aes(y = model_name, yend = model_name,
             color = model_name, group = model_name)) +
  geom_segment(aes(x = p025, xend = p975), size = 0.75) +
  geom_segment(aes(x = p10, xend = p90), size = 1.125) +
  geom_segment(aes(x = p25, xend = p75), size = 1.5) +
  geom_point(aes(x = p50), size = 2, shape = 3) +
  scale_x_continuous(minor_breaks = seq(0, 100, 2.5)) +
  scale_color_manual(values = param_colors) +
  labs(x = "MSY (Gg)") +
  theme_jkb(base_size = 9) +
  theme(axis.title.y = element_blank(),
        panel.grid.major.x = element_line(color = rgb(0, 0, 0, 0.15)),
        panel.grid.minor.x = element_line(color = rgb(0, 0, 0, 0.05))) +
  guides(color = FALSE)
fmsy_quant <- fmsy_summ %>%
  ggplot(aes(y = model_name, yend = model_name,
             color = model_name, group = model_name)) +
  geom_segment(aes(x = p025, xend = p975), size = 0.75) +
  geom_segment(aes(x = p10, xend = p90), size = 1.125) +
  geom_segment(aes(x = p25, xend = p75), size = 1.5) +
  geom_point(aes(x = p50), size = 2, shape = 3) +
  scale_x_continuous(minor_breaks = seq(0, 0.4, 0.025)) +
  scale_color_manual(values = param_colors) +
  labs(x = expression(F["MSY"])) +
  theme_jkb(base_size = 9) +
  theme(axis.title.y = element_blank(),
        panel.grid.major.x = element_line(color = rgb(0, 0, 0, 0.15)),
        panel.grid.minor.x = element_line(color = rgb(0, 0, 0, 0.05))) +
  guides(color = FALSE)
Pfinal_quant <- Pfinal_summ %>%
  ggplot(aes(y = model_name, yend = model_name,
             color = model_name, group = model_name)) +
  geom_segment(aes(x = p025, xend = p975), size = 0.75) +
  geom_segment(aes(x = p10, xend = p90), size = 1.125) +
  geom_segment(aes(x = p25, xend = p75), size = 1.5) +
  geom_point(aes(x = p50), size = 2, shape = 3) +
  scale_x_continuous(minor_breaks = seq(0, 1, 0.05)) +
  scale_color_manual(values = param_colors) +
  labs(x = "Depletion in 1990") +
  theme_jkb(base_size = 9) +
  theme(axis.title.y = element_blank(),
        panel.grid.major.x = element_line(color = rgb(0, 0, 0, 0.15)),
        panel.grid.minor.x = element_line(color = rgb(0, 0, 0, 0.05))) +
  guides(color = FALSE)
mgt_fig <- grid.arrange(msy_quant, fmsy_quant, Pfinal_quant, nrow = 3) #,
                        # top = "Posterior quantiles of management quantities")
ggsave("figs/fig3_mgt_quant.tiff", mgt_fig, width = 6, height = 6)
ggsave("figs/fig3_mgt_quant.pdf", mgt_fig, device = cairo_pdf,
       width = 6, height = 6)


## Consider the `ggridges` version
## msy_post %>%
##   mutate(model_name = factor(model_name, levels = rev(unique(model_name)))) %>%
##   ggplot(aes(x = MSY, y = model_name, fill = model_name)) +
##   geom_density_ridges() +
##   scale_fill_futurama() +
##   coord_cartesian(xlim = c(5, 40)) +
##   labs(title = "Posterior distribution of MSY under different parameterizations") +
##   theme_jkb(base_size = 9) +
##   theme(axis.title.y = element_blank()) +
##   guides(fill = FALSE)
## ggsave("figs/msy_post_ridges.png", width = 12, height = 10)

##==============================================================================
##-Varying Catch SD-------------------------------------------------------------
csd_datetime <- "2018-12-12 06:18:58"
load(paste0("results/", csd_datetime, "_csd.Rdata"))

## Diagnostics
diag_csd <- csd_df %>%
  mutate(ess = map(fit, ~ summary(.)$summary[, "n_eff"])) %>%
  transmute(catch_sd = catch_sd,
            td_total = map_dbl(fit, get_num_max_treedepth),
            div_total = map_dbl(fit, get_num_divergent),
            ## na.rm to get rid of fixed (Pmed[1]) pars with NaN ESS
            exp_ess = map_dbl(ess, mean, na.rm = TRUE),
            min_ess = map_dbl(ess, min, na.rm = TRUE),
            min_ess_par = map_chr(ess,
                                  function(p) attr(p, "names")[which.min(p)]),
            max_ess = map_dbl(ess, max, na.rm = TRUE))

## Potential plots for these diagnostics:
## - Diagnostic counts vs catch_sd
## - Sampling time vs catch_sd
## - ESS rate vs catch_sd

##-Table X: Diagnostics by catch_sd---------------------------------------------
## Edited manually for sigfigs etc!
knitr::kable(diag_csd)

## Fit timing
time_csd <- csd_df %>%
  left_join(diag_csd, by = "catch_sd") %>%
  mutate(times = map(fit, get_elapsed_time)) %>%
  transmute(catch_sd = catch_sd,
            warmup = map_dbl(times, ~ sum(.[, "warmup"])),
            sample = map_dbl(times, ~ sum(.[, "sample"])),
            min_ess_rate = min_ess / sample,
            exp_ess_rate = exp_ess / sample)

##-Table X: Timings by catch_sd-------------------------------------------------
time_csd %>%
  # filter(catch_sd < 4) %>%
  select(`Catch error` = catch_sd,
         `Min ESS Rate` = min_ess_rate) %>%
  knitr::kable(., digits = 3)


## Posterior densities - catches
c_post <- csd_df %>%
  filter(catch_sd < 1) %>%
  transmute(catch_sd = factor(signif(catch_sd, 2), levels = signif(catch_sd, 2)),
            `C_pred[1]` = map(fit, rstan::extract, pars = "C_pred[1]"),
            `C_pred[8]` = map(fit, rstan::extract, pars = "C_pred[8]"),
            `C_pred[16]` = map(fit, rstan::extract, pars = "C_pred[16]"),
            `C_pred[23]` = map(fit, rstan::extract, pars = "C_pred[23]")) %>%
  unnest() %>% unnest() %>%
  gather(c_year, c_est, -catch_sd) %>%
  mutate(c_year = factor(c_year, levels = c("C_pred[1]", "C_pred[8]",
                                            "C_pred[16]", "C_pred[23]")))

c_orig <- data_frame(c_year = factor(c("C_pred[1]", "C_pred[8]",
                                       "C_pred[16]", "C_pred[23]"),
                                     levels = c("C_pred[1]", "C_pred[8]",
                                                "C_pred[16]", "C_pred[23]")),
                     c_obs = tuna_data$C[c(1, 8, 16, 23)])

##-Catch posterior examples------------------------------------------------------
## Show that catch estimation behaves well with changing catch_sd. Needs a bit
## more negative space, and better facet titles.
c_post %>%
  ggplot(aes(x = c_est, color = catch_sd, fill = catch_sd)) +
  geom_density() +
  facet_wrap(~ c_year, scales = "free") +
  geom_vline(aes(xintercept = c_obs), data = c_orig,
             alpha = 0.5, size = 1, linetype = "dashed") +
  ## scale_color_simpsons() + scale_fill_simpsons() +
  scale_color_viridis(discrete = TRUE, option = "E") +
  scale_fill_viridis(discrete = TRUE, option = "E") +
  labs(# title = "Example catch posteriors",
       x = "Catch biomass", y = "Density",
       color = "Catch SD", fill = "Catch SD") +
  theme_jkb(base_size = 9)
ggsave("figs/catch_post.png", width = 12, height = 10)

## Posterior densities - Biomass
b_post <- csd_df %>%
  filter(catch_sd < 4) %>%
  transmute(catch_sd = factor(signif(catch_sd, 2), levels = signif(catch_sd, 2)),
            `Biomass[1]` = map(fit, rstan::extract, pars = "Biomass[1]"),
            `Biomass[8]` = map(fit, rstan::extract, pars = "Biomass[8]"),
            `Biomass[16]` = map(fit, rstan::extract, pars = "Biomass[16]"),
            `Biomass[23]` = map(fit, rstan::extract, pars = "Biomass[23]")) %>%
  unnest() %>% unnest() %>%
  gather(b_year, b_post, -catch_sd) %>%
  mutate(b_year = factor(b_year, levels = c("Biomass[1]", "Biomass[8]",
                                            "Biomass[16]", "Biomass[23]")))

##-Biomass posterior examples---------------------------------------------------
## Show that biomass posteriors don't change with catch_sd. Might be a better
## way to show this, e.g. below.
b_post %>%
  ggplot(aes(x = b_post, color = catch_sd, fill = catch_sd)) +
  geom_density(alpha = 0.25, size = 1) +
  facet_wrap(~ b_year, scales = "free") +
  scale_color_startrek() + scale_fill_startrek() +
  labs(# title = "Example biomass posteriors",
       x = "Biomass",
       y = "Density",
       fill = "Catch SD", color = "Catch SD") +
  theme_jkb(base_size = 9)
ggsave("figs/biomass_post_dens.png", width = 12, height = 10)

##-Figure X: Posterior biomass series
b_series <- csd_df %>%
  filter(catch_sd < 4) %>%
  transmute(catch_sd = catch_sd,
            bseries = map(fit, as.data.frame, pars = "Biomass")) %>%
  unnest() %>% unnest() %>%
  rename(!!varyear) %>%
  gather(year, biomass, -catch_sd) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(catch_sd, year) %>%
  summarize(p10 = quantile(biomass, 0.1),
            p25 = quantile(biomass, 0.25),
            p50 = median(biomass),
            p75 = quantile(biomass, 0.75),
            p90 = quantile(biomass, 0.9)) %>%
  ungroup()

b_series %>%
  mutate(catch_sd = round(catch_sd, 3),
         catch_sd = factor(catch_sd)) %>%
  ggplot(aes(x = year, y = p50,
             color = catch_sd, fill = catch_sd)) +
  geom_vline(xintercept = 1989.5, linetype = "dashed",
             size = 0.5, color = rgb(0, 0, 0, 0.15)) +
  geom_linerange(aes(ymin = p10, ymax = p90), size = 0.5,
                 position = bp_dodge) +
  geom_linerange(aes(ymin = p25, ymax = p75), size = 0.75,
                 position = bp_dodge) +
  geom_point(position = bp_dodge,
             shape = "-", size = 2.5, color = "black") +
  scale_color_viridis(discrete = TRUE, option = "E") +
  scale_fill_viridis(discrete = TRUE, option = "E") +
  annotate(geom = "text", x = 1989.6, y = 0, hjust = 0,
           label = "Projected",
           family = "Montserrat", size = 2, color = rgb(0, 0, 0, 0.15)) +
  labs(#title = "Biomass posteriors with varying catch errors",
       x = "Year", y = "Biomass (Gg)",
       color = "Catch error\n (std. dev.)", fill = "Catch error\n (std. dev.)") +
  coord_cartesian(ylim = c(0, 400)) +
  theme_jkb(base_size = 9) +
  theme(legend.position = c(0.925, 0.75))
ggsave("figs/fig4_csd_biomass.tiff", width = 6, height = 4)
ggsave("figs/fig4_csd_biomass.pdf", device = cairo_pdf,
       width = 6, height = 4)

## Posterior densities - MSY
## msy_post <- csd_df %>%
##   filter(catch_sd < 4) %>%
##   transmute(catch_sd = factor(catch_sd, levels = catch_sd),
##             MSY = map(fit, rstan::extract, pars = "MSY")) %>%
##   unnest() %>% unnest()

## msy_post %>%
##   ggplot(aes(x = MSY, color = catch_sd, fill = catch_sd)) +
##   geom_density(alpha = 0.2, size = 2) +
##   scale_color_viridis(discrete = TRUE, option = "E") +
##   scale_fill_viridis(discrete = TRUE, option = "E") +
##   labs(title = "Posterior distribution of MSY",
##        x = "MSY", y = "Density",
##        color = "Catch error\n (std. dev.)",
##        fill = "Catch error\n (std. dev.)") +
##   theme_jkb(base_size = 9) +
##   theme(legend.position = c(0.925, 0.8)) +
## ggsave("figs/msy_post.png", width = 12, height = 10)

## ## Posterior densities - FMSY
## fmsy_post <- csd_df %>%
##   filter(catch_sd < 4) %>%
##   transmute(catch_sd = factor(catch_sd, levels = catch_sd),
##             FMSY = map(fit, rstan::extract, pars = "FMSY")) %>%
##   unnest() %>% unnest()

## fmsy_post %>%
##   ggplot(aes(x = FMSY, color = catch_sd, fill = catch_sd)) +
##   geom_density(alpha = 0.2, size = 2) +
##   theme_jkb(base_size = 9)
## ggsave("figs/fmsy_post.png", width = 12, height = 10)

##-Figure 4: Parameterization management posteriors-----------------------------
msy_summ <- csd_df %>%
  filter(catch_sd < 4) %>%
  transmute(catch_sd = round(catch_sd, 3),
            catch_sd = factor(catch_sd),
            bseries = map(fit, as.data.frame, pars = "MSY")) %>%
  unnest() %>% unnest() %>%
  group_by(catch_sd) %>%
  summarize(p025 = quantile(MSY, 0.025),
            p10 = quantile(MSY, 0.1),
            p25 = quantile(MSY, 0.25),
            p50 = quantile(MSY, 0.5),
            p75 = quantile(MSY, 0.75),
            p90 = quantile(MSY, 0.9),
            p975 = quantile(MSY, 0.975))
fmsy_summ <- csd_df %>%
  filter(catch_sd < 4) %>%
  transmute(catch_sd = round(catch_sd, 3),
            catch_sd = factor(catch_sd),
            bseries = map(fit, as.data.frame, pars = "FMSY")) %>%
  unnest() %>% unnest() %>%
  group_by(catch_sd) %>%
  summarize(p025 = quantile(FMSY, 0.025),
            p10 = quantile(FMSY, 0.1),
            p25 = quantile(FMSY, 0.25),
            p50 = quantile(FMSY, 0.5),
            p75 = quantile(FMSY, 0.75),
            p90 = quantile(FMSY, 0.9),
            p975 = quantile(FMSY, 0.975))
Pfinal_summ <- csd_df %>%
  filter(catch_sd < 4) %>%
  transmute(catch_sd = round(catch_sd, 3),
            catch_sd = factor(catch_sd),
            bseries = map(fit, as.data.frame, pars = "P_final")) %>%
  unnest() %>% unnest() %>%
  group_by(catch_sd) %>%
  summarize(p025 = quantile(P_final, 0.025),
            p10 = quantile(P_final, 0.1),
            p25 = quantile(P_final, 0.25),
            p50 = quantile(P_final, 0.5),
            p75 = quantile(P_final, 0.75),
            p90 = quantile(P_final, 0.9),
            p975 = quantile(P_final, 0.975))

##-Figure X: Quantiles for comparison between parameterizations-----------------
msy_quant <- msy_summ %>%
  ggplot(aes(y = catch_sd, yend = catch_sd,
             color = catch_sd, group = catch_sd)) +
  geom_segment(aes(x = p025, xend = p975), size = 0.75) +
  geom_segment(aes(x = p10, xend = p90), size = 1.125) +
  geom_segment(aes(x = p25, xend = p75), size = 1.5) +
  geom_point(aes(x = p50), size = 2, shape = 3) +
  scale_x_continuous(minor_breaks = seq(0, 100, 1)) +
  scale_color_viridis(discrete = TRUE, option = "E") +
  labs(x = "MSY (Gg)") +
  theme_jkb(base_size = 9) +
  theme(axis.title.y = element_blank(),
        panel.grid.major.x = element_line(color = rgb(0, 0, 0, 0.15)),
        panel.grid.minor.x = element_line(color = rgb(0, 0, 0, 0.05))) +
  guides(color = FALSE)
fmsy_quant <- fmsy_summ %>%
  ggplot(aes(y = catch_sd, yend = catch_sd,
             color = catch_sd, group = catch_sd)) +
  geom_segment(aes(x = p025, xend = p975), size = 0.75) +
  geom_segment(aes(x = p10, xend = p90), size = 1.125) +
  geom_segment(aes(x = p25, xend = p75), size = 1.5) +
  geom_point(aes(x = p50), size = 2, shape = 3) +
  scale_x_continuous(minor_breaks = seq(0, 0.4, 0.025)) +
  scale_color_viridis(discrete = TRUE, option = "E") +
  labs(x = expression(F["MSY"])) +
  theme_jkb(base_size = 9) +
  theme(axis.title.y = element_blank(),
        panel.grid.major.x = element_line(color = rgb(0, 0, 0, 0.15)),
        panel.grid.minor.x = element_line(color = rgb(0, 0, 0, 0.05))) +
  guides(color = FALSE)
Pfinal_quant <- Pfinal_summ %>%
  ggplot(aes(y = catch_sd, yend = catch_sd,
             color = catch_sd, group = catch_sd)) +
  geom_segment(aes(x = p025, xend = p975), size = 0.75) +
  geom_segment(aes(x = p10, xend = p90), size = 1.125) +
  geom_segment(aes(x = p25, xend = p75), size = 1.5) +
  geom_point(aes(x = p50), size = 2, shape = 3) +
  scale_x_continuous(minor_breaks = seq(0, 1, 0.05)) +
  scale_color_viridis(discrete = TRUE, option = "E") +
  labs(x = "Depletion in 1990") +
  theme_jkb(base_size = 9) +
  theme(axis.title.y = element_blank(),
        panel.grid.major.x = element_line(color = rgb(0, 0, 0, 0.15)),
        panel.grid.minor.x = element_line(color = rgb(0, 0, 0, 0.05))) +
  guides(color = FALSE)
mgt_fig <- grid.arrange(msy_quant, fmsy_quant, Pfinal_quant, nrow = 3,
                        # top = "Posterior quantiles of management quantities",
                        left = "Catch error (std. dev.)")
ggsave("figs/fig5_csd_mgt.tiff", mgt_fig, width = 6, height = 6)
ggsave("figs/fig5_csd_mgt.pdf", device = cairo_pdf,
       mgt_fig, width = 6, height = 6)
