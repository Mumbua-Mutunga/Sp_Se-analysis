#############################################################################
library(Hmisc)
#############################################################################

# Calculate incidence
cases <- 20 # number of cases detected in a given population over a given unit of time
period <- 20/12 # amount of time during which surveillance was enhanced
population <- 150000 # study population (humans)
HDR <- 8 # estimated human dog ratio
dogs <- population/ HDR

inc_dogs <- (cases/period)/ dogs * 1000 # incidence per 1000 dogs per year
inc_dogs # NOTE this is the number of rabid dogs DETECTED! It will definitely be an underestimate of the true incidence

# 95% confidence intervals
binconf(cases, period*dogs, method="exact")*1000

# You could use this function to calculate dog rabies incidence in different settings
calc_inc <- function(cases, period, population, HDR){
  dogs <- population/ HDR
  inc_dogs <- binconf(cases, period*dogs, method="exact")*1000 # incidence per 1000 dogs per year with 95% CIs
  return(inc_dogs)
}

# test function
inc_est <- calc_inc(30, 15/12, 350000, 7)
inc_est

# Can you adapt this to just calculate dog rabies cases per 100,000 persons??

#############################################################################
# Estimate number of tests required nationwide:

# Apply incidence to whole dog popualtion of interest (i.e. countrywide)
population <- 35000000
HDR <- 20 # HDR probably varies widely from rural livestock owning populations with many dogs, to coastal muslim populations with much fewer
dogs <- population/ HDR 

# you could always adjust for vaccination coverage if you asume some populations are well vaccinated?
# but remember where your estimates of rabies incidence come from - was there some level of vaccination coverage in these counties?

# total rabid dogs in country = annual dog rabies incidence x dog population
RDT_need <- inc_dogs * dogs # note this is based on detected incidence so will be an underestimate of dog cases!

# compare to current testing levels
annual_cases <- 200 # lets say this is the number of cases detected annually at current labs
RDT_need/ annual_cases # what would be the required increase in testing needed to detect all RDT+ cases at this incidence level?

#############################################################################
# Estimate costs of testing under differnet approaches

# testing costs
RDT_cost <- 10
DFAT_cost <- 20 # not including lab capital equipment - assume these are sufficient for the country??
PCR_cost <- 25
sample_transport <- 15

# lets just estimate costs under a specific testing strategy:
RDT_sens <- 0.92 # consider that some clinically suspect animals test RDT negative (get this from your analysis)
QC_positives <- 0.05 # consider how many RDT+ samples you would want to QC with DFAT/PCR

# DFAT only under enhanced surveillance
DFAT_only <- RDT_need * (DFAT_cost + sample_transport) 

# RDT only under enhanced surveillance
RDT_only <- RDT_need * RDT_cost

# PCR only under enhanced surveillance
PCR_only <- RDT_need * (PCR_cost + sample_transport)

# RDT screening followed by DFAT confirmation of 5% of RDT+ samples & all negative samples?
RDT_DFAT <- (RDT_need * RDT_cost) + # baseline field testing
  (QC_positives * RDT_need * (DFAT_cost+sample_transport)) + # RDT quality control (remeber that RDTs do better than DFAT on poor quality samples ;)
  ((1-RDT_sens) * RDT_need * DFAT_cost) # look for false negatives

# RDT screening followed by PCR confirmation of 5% of RDT+ samples & all negative samples?
RDT_PCR <- (RDT_need * RDT_cost) + # baseline field testing
  (QC_positives * RDT_need * (PCR_cost+sample_transport)) + # RDT quality control (remeber that RDTs do better than DFAT on poor quality samples ;)
  ((1-RDT_sens) * RDT_need * PCR_cost) # look for false negatives

# compare strategies
DFAT_only 
PCR_only
RDT_only
RDT_DFAT
RDT_PCR

# function to estimate testing costs
calc_testing <- function(inc_dogs, population, HDR, test1_cost, test2_cost, QA, sens){
  dogs <- population/ HDR 
  test_need <- inc_dogs * dogs # numbers of rabid dogs to be test
  test1_only <- test_need * test1_cost # baseline field testing
  test2_only <- test_need * test2_cost # baseline field testing
  test1_QA <- (test_need * QA * test2_cost) + # quality assurance of test 1 incl. examining potential FNs
    ((1-sens) * test_need * test2_cost) 
  
  results <- data.frame(Strategy=c("Test 1 only", "Test 2 only", "Test 1 + Test 2"),
                        Cost=c(test1_only, test2_only, test1_only + test1_QA))
  return(results)
}

# test the function for an example county.....
calc_testing(inc_dogs = 0.5 /1000, population = 250000, HDR = 8, # remember to adjust for per 1000 dogs
             test1_cost=10, test2_cost=(20+15), # note test 2 includes transport
             QA=0.05, sens=0.92) 

calc_testing(inc_dogs = inc_est[1] / 1000, population = 250000, HDR = 8, #
             test1_cost=10, test2_cost=(20+15), # note test 2 includes transport
             QA=0.05, sens=0.92) 

calc_testing(inc_dogs = inc_est[3] / 1000, population = 250000, HDR = 8, # look at upper limit?
             test1_cost=10, test2_cost=(20+15), # note test 2 includes transport
             QA=0.05, sens=0.92) 

calc_testing(inc_dogs = inc_est[1] / 1000, population = 67000000, HDR = 25, # look at entire country?
             test1_cost=10, test2_cost=(20+15), # note test 2 includes transport
             QA=0.05, sens=0.92) 

# how much more expensive is the current testing level vs an RDT adapted strategy?
# How many cases currently get detected under current testing levels?
# what is the typical time for current testing (and therefore how is current practice - PEP, MDV, etc influenced by testing?)
# How could this change under alternative testing strategies?
# Note whether transport costs might change in different settings?
# Is current testing fit for purpose?
# what would you advocate in terms of RDT use?
# What are the implications of sticking with a laboratory-only based approach vs field testing too?

