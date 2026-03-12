library(Hmisc)
library(ggplot2)
library(dplyr)

# ── Helper function ───────────────────────────────────────────────────────────
# Returns a named numeric vector: estimate, lower, upper
calc_inc <- function(cases, period, population, HDR) {
  dogs <- population / HDR
  ci   <- binconf(cases, period * dogs, method = "exact") * 1000
  c(estimate = ci[1], lower = ci[2], upper = ci[3])
}

# ── County-level incidence (per 1,000 dogs/year) ─────────────────────────────
inc_makueni <- calc_inc(102, 28/12, 987653, 8)
inc_siaya   <- calc_inc(93,  12/12, 993183, 7)

# ── National incidence proxy (mean of two counties) ──────────────────────────
inc_national <- c(
  estimate = mean(c(inc_makueni["estimate"], inc_siaya["estimate"])) / 1000,
  lower    = mean(c(inc_makueni["lower"],    inc_siaya["lower"]))    / 1000,
  upper    = mean(c(inc_makueni["upper"],    inc_siaya["upper"]))    / 1000
)

# ── National parameters ───────────────────────────────────────────────────────
population_national <- 47564296  # KNBS 2019 census
HDR_national        <- 10
dogs_national       <- population_national / HDR_national

# Testing need (lower, estimate, upper)
RDT_need <- inc_national * dogs_national

# ── Unit costs (USD) ──────────────────────────────────────────────────────────
RDT_cost         <- 12
DFAT_cost        <- 18
PCR_cost         <- 35
sample_transport <- 3.5
RDT_sens         <- 0.9375
QC_fraction      <- 0.05

# ── Cost function ─────────────────────────────────────────────────────────────
calc_costs <- function(need) {
  c(
    "DFAT only"  = need * (DFAT_cost + sample_transport),
    "PCR only"   = need * (PCR_cost  + sample_transport),
    "RDT only"   = need * RDT_cost,
    "RDT + DFAT" = (need * RDT_cost) +
      (QC_fraction * need * (DFAT_cost + sample_transport)) +
      ((1 - RDT_sens) * need * DFAT_cost),
    "RDT + PCR"  = (need * RDT_cost) +
      (QC_fraction * need * (PCR_cost + sample_transport)) +
      ((1 - RDT_sens) * need * PCR_cost)
  )
}

# ── Build cost dataframe ──────────────────────────────────────────────────────
cost_df <- data.frame(
  Strategy = names(calc_costs(1)),
  Cost     = calc_costs(RDT_need["estimate"]),
  Lower    = calc_costs(RDT_need["lower"]),
  Upper    = calc_costs(RDT_need["upper"]),
  row.names = NULL
)

# ── Print summary ─────────────────────────────────────────────────────────────
cat("── Estimated annual testing need (national) ──\n")
cat(sprintf("Lower: %s | Estimate: %s | Upper: %s tests/year\n\n",
            format(round(RDT_need["lower"]),    big.mark = ","),
            format(round(RDT_need["estimate"]), big.mark = ","),
            format(round(RDT_need["upper"]),    big.mark = ",")))

cat("── Cost summary (USD) ──\n")
print(cost_df |> mutate(across(c(Cost, Lower, Upper), ~ round(.x, 0))))

# ── Plot ──────────────────────────────────────────────────────────────────────
figure10 <- ggplot(cost_df, aes(x = reorder(Strategy, Cost),
                                y = Cost, fill = Strategy)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.3) +
  coord_flip() +
  labs(x = NULL, y = "Total cost (USD)") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none")

figure10

ggsave("figs/figure10_testing_costs.png",
       figure10, width = 15, height = 10, dpi = 300)
