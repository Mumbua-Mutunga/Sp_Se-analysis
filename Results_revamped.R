library(Hmisc)
library(ggplot2)
library(dplyr)
library(patchwork)

# ── Shared parameters ────────────────────────────────────────────────────────
county_params <- tibble(
  County        = c("Makueni", "Siaya"),
  population    = c(987654, 993183),
  HDR           = c(8, 7),
  cases         = c(102, 93),
  period_months = c(28, 12)
)

# ── Helper functions ─────────────────────────────────────────────────────────

# Incidence per 1,000 dogs per year
calc_inc_dogs <- function(cases, period_months, population, HDR) {
  dogs         <- population / HDR
  period_years <- period_months / 12
  ci           <- binconf(cases, period_years * dogs, method = "exact") * 1000
  # binconf returns: PointEst, Lower, Upper
  tibble(estimate = ci[1], lower = ci[2], upper = ci[3])
}

# Detected rabid dogs per 100,000 people per year
calc_inc_people <- function(cases, period_months, population) {
  period_years <- period_months / 12
  ci           <- binconf(cases, period_years * population, method = "exact") * 100000
  tibble(estimate = ci[1], lower = ci[2], upper = ci[3])
}

# ── Compute estimates ────────────────────────────────────────────────────────
inc_dogs <- county_params |>
  rowwise() |>
  mutate(calc_inc_dogs(cases, period_months, population, HDR)) |>
  ungroup()

inc_people <- county_params |>
  rowwise() |>
  mutate(calc_inc_people(cases, period_months, population)) |>
  ungroup()

# ── Plots ────────────────────────────────────────────────────────────────────
plot_theme <- theme_bw(base_size = 18) + theme(legend.position = "none")

p_people <- ggplot(inc_people, aes(x = County, y = estimate, fill = County)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(
    title = "Estimated detected rabid dogs per 100,000 people per year",
    y = "Cases per 100,000 people", x = NULL
  ) +
  plot_theme

p_dogs <- ggplot(inc_dogs, aes(x = County, y = estimate, fill = County)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(
    title = "Estimated annual dog rabies incidence by county",
    y = "Incidence per 1,000 dogs per year", x = NULL
  ) +
  plot_theme

# ── Combine & save ───────────────────────────────────────────────────────────
figure7 <- p_people | p_dogs

figure7

ggsave("figs/figure7_dog_rabies_incidence.png",
       figure7, width = 18, height = 12, dpi = 300)

# ── Print estimates for reporting ────────────────────────────────────────────
cat("--- Detected rabid dogs per 100,000 people per year ---\n")
print(inc_people |> select(County, estimate, lower, upper))

cat("\n--- Dog rabies incidence per 1,000 dogs per year ---\n")
print(inc_dogs |> select(County, estimate, lower, upper))
