library(Hmisc)
library(ggplot2)
library(dplyr)
library(patchwork)

# Function
calc_inc <- function(cases, period, population, HDR){
  dogs <- population / HDR
  ci <- binconf(cases, period * dogs, method = "exact") * 1000
  data.frame(lower = ci[1], estimate = ci[2], upper = ci[3])
}

# Data
makueni <- 987654
siaya <- 993183
HDR_makueni <- 8
HDR_siaya <- 7
cases_makueni <- 102
cases_siaya <- 93
makueni_months <- 28
siaya_months <- 12

inc_makueni <- calc_inc(cases_makueni, makueni_months/12, makueni, HDR_makueni) |> mutate(County="Makueni")
inc_siaya   <- calc_inc(cases_siaya, siaya_months/12, siaya, HDR_siaya) |> mutate(County="Siaya")
inc_df <- bind_rows(inc_makueni, inc_siaya)

# Plot
dog_inc_plot<- ggplot(inc_df, aes(x = County, y = estimate, fill = County)) +
  geom_bar(stat="identity", color="black", width=0.6) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  labs(
    title = "Estimated annual dog rabies incidence by county",
    y = "Incidence per 1 000 dogs per year", x = NULL
  ) +
  theme_bw(base_size=18) +
  theme(legend.position="none")


#What it shows:
#Siaya has roughly double the detected incidence of Makueni.
#Wide CIs underscore under-detection and uncertainty in passive surveillance.


# Figure 2: Estimated Dog Rabies Incidence per 100 000 People -------------


calc_inc_bites <- function(cases, period, population){
  ci <- binconf(cases, period * population, method="exact") * 100000
  data.frame(lower = ci[1], estimate = ci[2], upper = ci[3])
}

bite_df <- bind_rows(
  calc_inc_bites(cases_makueni, makueni_months/12, makueni) |> mutate(County="Makueni"),
  calc_inc_bites(cases_siaya, siaya_months/12, siaya) |> mutate(County="Siaya")
)

dog_to_human <- ggplot(bite_df, aes(x = County, y = estimate, fill = County)) +
  geom_bar(stat="identity", color="black", width=0.6) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  labs(
    title = "Estimated detected rabid dogs per 100 000 people per year",
    y = "Cases per 100 000 people", x = NULL
  ) +
  theme_bw(base_size=18) +
  theme(legend.position="none")

#Interpretation:
#Gives a human-scale sense of risk and surveillance intensity.
#Useful for comparing to bite incidence or PEP demand.

# Combine vertically without a title
combined_plot <- dog_to_human | dog_inc_plot

# Display combined figure
combined_plot

# Save the figure
ggsave("combined_dog_rabies_figure.png",
       combined_plot, width = 18, height = 12, dpi = 300)



# Figure 3: National Testing Cost by Strategy -----------------------------

# Parameters
RDT_cost <- 12 #converted Euro to USD approximate
DFAT_cost <- 18 #converted Euro to USD approximate
PCR_cost <- 35 #converted Euro to USD approximate
sample_transport <- 3.5 #average of the prices we used for makueni and siaya
RDT_sens <- 0.92
QC_positives <- 0.05
population <- 47564296 #KNBS 2019 census
HDR <- 10
inc_dogs <- 0.3  # per 1000 dogs/year (illustrative)

dogs <- population / HDR
RDT_need <- inc_dogs * dogs

DFAT_only <- RDT_need * (DFAT_cost + sample_transport)
PCR_only  <- RDT_need * (PCR_cost + sample_transport)
RDT_only  <- RDT_need * RDT_cost
RDT_DFAT  <- (RDT_need * RDT_cost) +
  (QC_positives * RDT_need * (DFAT_cost+sample_transport)) +
  ((1-RDT_sens) * RDT_need * DFAT_cost)
RDT_PCR   <- (RDT_need * RDT_cost) +
  (QC_positives * RDT_need * (PCR_cost+sample_transport)) +
  ((1-RDT_sens) * RDT_need * PCR_cost)

cost_df <- data.frame(
  Strategy = c("DFAT only", "PCR only", "RDT only", "RDT + DFAT", "RDT + PCR"),
  Cost_USD = c(DFAT_only, PCR_only, RDT_only, RDT_DFAT, RDT_PCR)
)

testing_costs <- ggplot(cost_df, aes(x = reorder(Strategy, Cost_USD), y = Cost_USD/1e6, fill = Strategy)) +
  geom_bar(stat="identity", color="black", width=0.7) +
  coord_flip() +
  labs(
    #title = "Estimated national annual testing costs by strategy",
    x = NULL, y = "Total cost (USD millions)"
  ) +
  theme_bw(base_size=18) +
  theme(legend.position="none")

ggsave("testing_costs.png",
       testing_costs, width = 15, height = 10, dpi = 300)

#Interpretation:
  #RDT + DFAT provides a cost-effective balance between accuracy and affordability.
  #PCR-only is the most expensive; RDT-only is cheapest but lacks confirmatory assurance.


# Figure 4: Required Increase in Testing Levels ---------------------------

# Assume earlier parameters
population <- 47564296
HDR <- 10
dogs <- population / HDR
inc_dogs <- 0.3  # per 1 000 dogs per year (detected incidence example)

RDT_need <- inc_dogs * dogs  # estimated rabid dogs detected under enhanced surveillance
annual_cases <- 1940          # currently detected nationally
testing_increase <- RDT_need / annual_cases

testing_df <- data.frame(
  Category = c("Current testing capacity", "Estimated tests required"),
  Tests = c(annual_cases, RDT_need)
)

# Plot
ggplot(testing_df, aes(x = Category, y = Tests, fill = Category)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  scale_y_log10(labels = scales::comma_format()) +
  geom_text(aes(label = format(round(Tests, 0), big.mark = ",")),
            vjust = -0.3, size = 4.2) +
  labs(
    title = "Required increase in testing to detect all RDT-positive dog rabies cases",
    y = "Number of tests (log scale)",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")


