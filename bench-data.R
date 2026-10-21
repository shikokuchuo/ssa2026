# Tables behind outline.md, from the benchmark's results files.
# Source: ~/r/tidymodels-parallel-benchmark (benchmark.qmd has the figure code).
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

bench_dir <- "../tidymodels-parallel-benchmark/results"
timings <- readRDS(file.path(bench_dir, "timings.rds"))
monitors <- readRDS(file.path(bench_dir, "monitors.rds"))
meta <- readRDS(file.path(bench_dir, "meta.rds"))
sweep_raw <- readRDS(file.path(bench_dir, "sweep.rds"))
workers <- meta$workers

wl_label <- c(
  boot_metrics = "A: Bootstrap metrics",
  resample_fit = "B: Resampled model fits",
  perm_importance = "C: Permutation importance",
  boot_pred_interval = "D: Bootstrap prediction intervals"
)
# future shows as "fescue" in the deck (a codename): anonymised until its
# author answers the questions we sent about these results.
backend_label <- c(
  sequential = "sequential",
  future = "fescue",
  mirai = "mirai",
  mizu = "mizu"
)

amdahl <- function(n, p) 1 / ((1 - p) + p / n)
implied_p <- function(s, n) (1 - 1 / s) / (1 - 1 / n)

# Main run: 12-worker speed-up, implied parallel fraction, ceiling (F3, F5)
baseline <- timings |>
  filter(backend == "sequential") |>
  summarize(baseline = mean(elapsed), .by = workload)

runs <- timings |>
  filter(backend != "sequential") |>
  inner_join(baseline, by = "workload") |>
  mutate(speed_up = baseline / elapsed)

fits <- runs |>
  filter(!monitored) |>
  summarize(
    mean_elapsed = mean(elapsed),
    mean_speedup = mean(speed_up),
    .by = c(workload, backend)
  ) |>
  mutate(
    p = implied_p(mean_speedup, workers),
    serial_pct = 100 * (1 - p),
    ceiling = 1 / (1 - p),
    workload = factor(wl_label[workload], levels = wl_label),
    backend = factor(backend_label[backend], levels = backend_label)
  ) |>
  arrange(workload, desc(mean_speedup))

# mirai with everywhere() on D (the honest counterweight)
everywhere_d <- tibble(
  export_s = meta$extras$boot_pred_interval$export,
  map_s = meta$extras$boot_pred_interval$map,
  speed_up = baseline$baseline[baseline$workload == "boot_pred_interval"] /
    meta$extras$boot_pred_interval$map
)

# Sweep: Karp–Flatt implied serial fraction at 2/4/8/12 workers (F6)
sweep_base <- sweep_raw$timings |>
  filter(backend == "sequential") |>
  summarize(baseline = mean(elapsed), .by = workload)

karp_flatt <- sweep_raw$timings |>
  filter(backend != "sequential") |>
  inner_join(sweep_base, by = "workload") |>
  mutate(speed_up = baseline / elapsed) |>
  summarize(speed_up = mean(speed_up), .by = c(workload, backend, workers)) |>
  mutate(
    serial_pct = 100 * (1 - implied_p(speed_up, workers)),
    workload = factor(wl_label[workload], levels = wl_label),
    backend = factor(backend_label[backend], levels = backend_label)
  ) |>
  arrange(workload, backend, workers)

# Busy worker cores over time, per monitored run (F4, F7)
busy <- monitors |>
  filter(pid != main_pid) |>
  mutate(rel_time = as.numeric(time - min(time)), .by = c(workload, backend)) |>
  summarize(cores = sum(pct_cpu) / 100, .by = c(workload, backend, rel_time)) |>
  mutate(
    workload = factor(wl_label[workload], levels = wl_label),
    backend = factor(backend_label[backend], levels = backend_label)
  )

if (sys.nframe() == 0L) {
  cat("workers:", workers, " cores:", meta$cores, " ", meta$r_version, "\n\n")
  print(as.data.frame(fits), digits = 3)
  cat("\nmirai everywhere() on D:\n")
  print(everywhere_d)
  cat("\nKarp-Flatt sweep:\n")
  print(as.data.frame(karp_flatt), digits = 3)
}
