## Tests for the ward simulation model in Simulation_model_code/
##
## Run from the repository root with:
##   testthat::test_file("tests/test_simulation.R")

library(testthat)
local_edition(3)

## Locate the model code whether the working directory is the repo root or tests/
code_dir <- if (dir.exists("../Simulation_model_code")) "../Simulation_model_code" else "Simulation_model_code"
for (f in c("Arrival Patient Attributes.R", "Infection Times.R", "Initial Patient Attributes.R",
            "Ward Simulation.R", "Simulation Metrics.R")) {
  source(file.path(code_dir, f))
}

## Helpers ---------------------------------------------------------------------

## Patient attributes where nobody ever leaves or progresses, so scenarios stay deterministic
inf_attrs <- function(bed_status) {
  m <- matrix(Inf, nrow = nrow(bed_status), ncol = ncol(bed_status))
  rep(list(m), 6)
}

## New patient attributes with no scheduled arrivals
no_arrivals <- function() rep(list(NULL), 7)

## Scheduled arrivals at the given times, who never leave or progress
scheduled_arrivals <- function(times) {
  c(list(times), rep(list(rep(Inf, length(times))), 6))
}

metric_names <- c("Focus Bay infections", "Other bay infections", "New arrivals", "Blocked arrivals",
                  "Unused Bed Days in the focus bay", "Unused bed days elsewhere in the ward",
                  "Outbreak occurred", "Infectious discharges", "Infectious transfers",
                  "Infected discharges", "Infected transfers", "Days of delayed discharges",
                  "Discharges", "Transfers")

example_bed_status <- rbind(c("Susceptible", "Susceptible"),
                            c("Infectious", "Infectious"),
                            c("Susceptible", "Susceptible"))

## A. Seeded regression tests (expected values from Example runs.R) -----------

test_that("arrival_patient_attributes matches the recorded seeded example", {
  set.seed(100)
  res <- arrival_patient_attributes(stopping_time = 5, arrival_rate = 1, transfer_rates = c(1/4, 1/4),
                                    discharge_rates = c(1/5, 1/5), recovery_rates = c(1/2, 1/3),
                                    discharge_SD = c(1, 1), transfer_SD = c(1, 1), latent_SD = 1,
                                    LOS_distribution = "Gamma", latent_distribution = "Gamma")
  expected <- list(
    c(0.9242116, 1.6480488, 1.7526937, 4.8500560),
    c(4.441285, 4.563849, 2.997607, 3.997388),
    c(4.871021, 6.711854, 6.186192, 5.232374),
    c(1.454069, 2.060742, 1.248095, 2.182870),
    c(3.3697398, 3.3931429, 1.1417432, 0.2148669),
    rep(Inf, 4),
    rep(Inf, 4)
  )
  expect_equal(res, expected, tolerance = 1e-6)
})

test_that("infection_times_sample matches the recorded seeded example", {
  set.seed(1000)
  res <- infection_times_sample(current_time = 0, example_bed_status, rbind(0.05, 0.1, 0.5, 0.2),
                                focus_bay = 2, n_infectious_states = 1, spread_to_focus_bay = TRUE)
  expected <- matrix(c(4.018648, Inf, 5.416410,
                       2.070772, Inf, 4.807220), nrow = 3)
  expect_equal(res, expected, tolerance = 1e-6)
})

test_that("initial_patient_attributes matches the recorded seeded example", {
  set.seed(100)
  res <- initial_patient_attributes(example_bed_status, n_infectious_states = 1, transfer_rates = c(1/4, 1/4),
                                    discharge_rates = c(1/5, 1/5), recovery_rates = c(1/2, 1/3),
                                    discharge_SD = c(1, 1), transfer_SD = c(1, 1), latent_SD = 1,
                                    LOS_distribution = "Gamma", latent_distribution = "Gamma")
  inf_matrix <- matrix(Inf, 3, 2)
  expected <- list(
    matrix(c(4.441285, 4.563849, 2.997607, 3.997388, 3.846199, 5.712800), nrow = 3),
    matrix(c(6.186192, 5.232374, 3.849565, 5.685571, 5.162760, 5.695539), nrow = 3),
    matrix(c(1.3117667, 0.5809215, 2.6778096, 1.861127, 2.060742, 1.248095), nrow = 3),
    matrix(c(1.014130, 6.069575, 3.369740, 3.3931429, 1.1417432, 0.2148669), nrow = 3),
    inf_matrix,
    inf_matrix
  )
  expect_equal(res, expected, tolerance = 1e-6)
})

test_that("ward_simulation_paired matches the recorded seeded example", {
  infection_rates <- rbind(0.05, 0.1, 0.5, 0.2)

  set.seed(100)
  init_attrs <- initial_patient_attributes(example_bed_status, n_infectious_states = 1, transfer_rates = c(1/4, 1/4),
                                           discharge_rates = c(1/5, 1/5), recovery_rates = c(1/2, 1/3),
                                           discharge_SD = c(1, 1), transfer_SD = c(1, 1), latent_SD = 1,
                                           LOS_distribution = "Gamma", latent_distribution = "Gamma")
  set.seed(100)
  infection_times <- infection_times_sample(current_time = 0, example_bed_status, infection_rates, focus_bay = 2,
                                            n_infectious_states = 1, spread_to_focus_bay = TRUE)
  set.seed(100)
  new_attrs <- arrival_patient_attributes(stopping_time = 5, arrival_rate = 1, transfer_rates = c(1/4, 1/4),
                                          discharge_rates = c(1/5, 1/5), recovery_rates = c(1/2, 1/3),
                                          discharge_SD = c(1, 1), transfer_SD = c(1, 1), latent_SD = 1,
                                          LOS_distribution = "Gamma", latent_distribution = "Gamma")
  set.seed(100)
  res <- ward_simulation_paired(focus_bay = 2, n_infectious_states = 1, infection_rates, time_seq = 5,
                                example_bed_status, init_attrs, infection_times,
                                restrictions = FALSE, quarantine_time = 0, close_bay_coef = 0,
                                spread_to_focus_bay = FALSE, queue_departures = TRUE, new_attrs)

  expected <- c(0, 2, 1, 3, 1.589952, 3.413776, 1, 0, 1, 0, 1, 0, 0, 5)
  expect_equal(unname(res[1, ]), expected, tolerance = 1e-6)
})

## B. Property tests --------------------------------------------------------------

test_that("arrival_patient_attributes returns consistent, sorted arrivals within the horizon", {
  set.seed(1)
  res <- arrival_patient_attributes(stopping_time = 30, arrival_rate = 2, transfer_rates = c(1/4, 1/4),
                                    discharge_rates = c(1/5, 1/5), recovery_rates = c(1/2, 1/3))
  expect_length(res, 7)
  n <- length(res[[1]])
  expect_gt(n, 0)
  expect_true(all(lengths(res) == n))
  expect_false(is.unsorted(res[[1]]))
  expect_true(all(res[[1]] <= 30))
  ## Only one infectious state, so the 2nd and 3rd infectious periods are infinite
  expect_true(all(is.infinite(res[[6]])))
  expect_true(all(is.infinite(res[[7]])))
})

test_that("arrival_patient_attributes returns NULLs when nobody arrives", {
  set.seed(1)
  res <- arrival_patient_attributes(stopping_time = 5, arrival_rate = 1e-12, transfer_rates = c(1/4, 1/4),
                                    discharge_rates = c(1/5, 1/5), recovery_rates = c(1/2, 1/3))
  expect_length(res, 7)
  expect_true(all(vapply(res, is.null, logical(1))))
})

test_that("initial_patient_attributes gives Inf for empty and missing beds", {
  bed_status <- rbind(c("Susceptible", "No Bed"),
                      c("Infectious", "Empty"),
                      c("Latent", "Susceptible"))
  set.seed(1)
  res <- initial_patient_attributes(bed_status, n_infectious_states = 1, transfer_rates = c(1/4, 1/4),
                                    discharge_rates = c(1/5, 1/5), recovery_rates = c(1/2, 1/3))
  unoccupied <- bed_status %in% c("Empty", "No Bed")
  expect_length(res, 6)
  for (m in res) {
    expect_equal(dim(m), dim(bed_status))
    expect_true(all(is.infinite(m[unoccupied])))
  }
  ## Occupied beds have finite transfer and discharge times
  expect_true(all(is.finite(res[[1]][!unoccupied])))
  expect_true(all(is.finite(res[[2]][!unoccupied])))
})

test_that("infection_times_sample only schedules infections for susceptible patients", {
  bed_status <- rbind(c("Susceptible", "No Bed"),
                      c("Infectious", "Susceptible"),
                      c("Latent", "Empty"))
  for (spread in c(TRUE, FALSE)) {
    set.seed(1)
    res <- infection_times_sample(current_time = 10, bed_status, rbind(0.05, 0.1, 0.5, 0.2),
                                  focus_bay = 2, n_infectious_states = 1, spread_to_focus_bay = spread)
    expect_true(all(is.infinite(res[bed_status != "Susceptible"])))
    expect_true(all(res[bed_status == "Susceptible"] >= 10))
  }
})

test_that("infection_times_sample gives no infections when all rates are zero", {
  set.seed(1)
  res <- infection_times_sample(current_time = 0, example_bed_status, rbind(0, 0, 0, 0),
                                focus_bay = 2, n_infectious_states = 1, spread_to_focus_bay = TRUE)
  expect_true(all(is.infinite(res)))
})

test_that("simulation_ward_metrics returns cumulative metrics with one row per stopping time", {
  for (seed in 1:10) {
    set.seed(seed)
    res <- simulation_ward_metrics(ward_configuration = c(4, 6, 6, 6, 6), bay_occupants = c(2, 4, 3, 4, 5),
                                   n_infectious_states = 1, infection_distribution = rep(1/4, 4),
                                   arrival_rate = 1, transfer_rates = c(1/4, 1/4), discharge_rates = c(1/5, 1/5),
                                   infection_rates = rbind(0.05, 0.1, 0.5, 0.2), recovery_rates = c(1/2, 1/3),
                                   quarantine_time = 3, spread_coef = 0.4, close_bay_coef = 0.5,
                                   time_seq = c(7, 14, 21), LOS_distribution = "Gamma",
                                   restrictions = TRUE, queue_departures = TRUE, spread_to_focus_bay = FALSE)
    expect_equal(dim(res), c(3, 14))
    expect_equal(colnames(res), metric_names)
    ## Every metric is cumulative, so it never decreases between stopping times
    expect_true(all(diff(res) >= 0), info = paste("seed", seed))
  }
})

## C. Tests for previously fixed bugs ------------------------------------------

test_that("the model runs with 2 and 3 infectious states", {
  for (n in 2:3) {
    for (seed in 1:5) {
      set.seed(seed)
      res <- simulation_ward_metrics(ward_configuration = c(4, 6, 6), bay_occupants = c(2, 6, 3),
                                     n_infectious_states = n, infection_distribution = rep(1 / (3 + n), 3 + n),
                                     arrival_rate = 1, transfer_rates = c(1/4, 1/4), discharge_rates = c(1/5, 1/5),
                                     infection_rates = matrix(c(0.05, 0.1, 0.5), nrow = 3, ncol = n),
                                     recovery_rates = c(1/2, rep(1/3, n)),
                                     quarantine_time = 3, spread_coef = 0.4, close_bay_coef = 0.5,
                                     time_seq = c(7, 14), LOS_distribution = "Exponential",
                                     restrictions = TRUE, queue_departures = TRUE, spread_to_focus_bay = FALSE)
      expect_equal(dim(res), c(2, 14), info = paste("states", n, "seed", seed))
    }
  }
})

test_that("infections in a focus bay in the last row are counted as focus bay infections", {
  ## Two rows: side rooms, then the focus bay. Before the bay_of() fix, row n_bay mapped to bay 0.
  bed_status <- rbind(c("Susceptible", "No Bed"),
                      c("Infectious", "Susceptible"))
  infection_rates <- rbind(0, 0, 1, 0)  # only within-bay spread
  set.seed(1)
  infection_times <- infection_times_sample(0, bed_status, infection_rates, focus_bay = 2,
                                            n_infectious_states = 1, spread_to_focus_bay = FALSE)
  res <- ward_simulation_paired(focus_bay = 2, n_infectious_states = 1, infection_rates, time_seq = 1000,
                                bed_status, inf_attrs(bed_status), infection_times,
                                new_patient_attributes = no_arrivals())
  expect_equal(unname(res[1, "Focus Bay infections"]), 1)
  expect_equal(unname(res[1, "Other bay infections"]), 0)
  expect_equal(unname(res[1, "Outbreak occurred"]), 0)
})

test_that("arrivals can be placed in bays other than the last row", {
  ## Five rows: side rooms, focus bay and bay 5 are full; only bays 3 and 4 have space.
  ## Before the bay_of() fix, beds in bays 3 and 4 were never offered to arrivals.
  bed_status <- rbind(c("Susceptible", "Susceptible"),
                      c("Susceptible", "Susceptible"),
                      c("Empty", "Empty"),
                      c("Empty", "Empty"),
                      c("Susceptible", "Susceptible"))
  infection_rates <- rbind(0, 0, 0, 0)
  set.seed(1)
  res <- ward_simulation_paired(focus_bay = 2, n_infectious_states = 1, infection_rates, time_seq = 10,
                                bed_status, inf_attrs(bed_status), matrix(Inf, 5, 2),
                                new_patient_attributes = scheduled_arrivals(c(1, 2)))
  expect_equal(unname(res[1, "New arrivals"]), 2)
  expect_equal(unname(res[1, "Blocked arrivals"]), 0)
  expect_equal(unname(res[1, "Unused Bed Days in the focus bay"]), 0)
})
