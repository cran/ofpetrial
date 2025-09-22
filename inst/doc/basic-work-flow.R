## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ofpetrial)

## -----------------------------------------------------------------------------
#--- for nitrogen ---#
n_plot_info <-
  prep_plot(
    input_name = "NH3",
    unit_system = "imperial",
    machine_width = 30,
    section_num = 1,
    harvester_width = 30,
    plot_width = 30
  )

dplyr::glimpse(n_plot_info)

#--- for seed ---#
seed_plot_info <-
  prep_plot(
    input_name = "seed",
    unit_system = "imperial",
    machine_width = 60,
    section_num = 24,
    harvester_width = 30,
    plot_width = 30
  )

dplyr::glimpse(seed_plot_info)

## -----------------------------------------------------------------------------
input_plot_info <- list(n_plot_info, seed_plot_info)

exp_data <-
  make_exp_plots(
    input_plot_info = input_plot_info,
    boundary_data = system.file("extdata", "boundary-simple1.shp", package = "ofpetrial"),
    abline_data = system.file("extdata", "ab-line-simple1.shp", package = "ofpetrial"),
    abline_type = "free"
  )

## -----------------------------------------------------------------------------
exp_data$exp_plots

## -----------------------------------------------------------------------------
exp_data$exp_plots[[1]]

## -----------------------------------------------------------------------------
viz(exp_data, type = "layout", abline = TRUE)

## -----------------------------------------------------------------------------
#!===========================================================
# ! Assign rates
# !===========================================================
n_rate_info <-
  prep_rate(
    plot_info = n_plot_info,
    gc_rate = 180,
    unit = "lb",
    rates = c(100, 140, 180, 220, 260),
    design_type = "ls",
    rank_seq_ws = c(5, 4, 3, 2, 1)
  )

dplyr::glimpse(n_rate_info)

seed_rate_info <-
  prep_rate(
    plot_info = seed_plot_info,
    gc_rate = 32000,
    unit = "seed",
    min_rate = 16000,
    max_rate = 40000,
    num_rates = 4,
    design_type = "ls"
  )

dplyr::glimpse(seed_rate_info)

## -----------------------------------------------------------------------------
trial_design <- assign_rates(exp_data, rate_info = list(n_rate_info, seed_rate_info))

## -----------------------------------------------------------------------------
viz(trial_design, abline = TRUE)

## -----------------------------------------------------------------------------
(
  cor_inputs <- check_ortho_inputs(trial_design)
)

## ----eval = FALSE-------------------------------------------------------------
# make_trial_report(
#   td = trial_design,
#   folder_path = getwd()
# )

## ----eval = FALSE-------------------------------------------------------------
# write_trial_files(td, folder_path = getwd(), zip = TRUE, zip_name = "td-collection")

