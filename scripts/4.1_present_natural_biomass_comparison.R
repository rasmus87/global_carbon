# Minor script to compare with other paper
# Can be run as a separate section
# 29/07-2021 Rasmus Ø Pedersen

# Load libraries
library(tidyverse)


# Total global biomass in carbon ------------------------------------------

## Conversion factor between wet weight and carbon mass
# Following:
# Y. M. Bar-On, R. Phillips, R. Milo, The biomass distribution on Earth. Proc. Natl. Acad. Sci. U. S. A. 115, 6506–6511 (2018).
# Here:
# https://milo-lab.github.io/biomass_distribution/animals/chordates/wild_mammals/wild_mammal.html#Estimating-the-biomass-of-wild-land-mammals

# Factor:
wet_to_c = 0.15


## Compare with Barnosky 2008 wild biomass pre humans
# Leftmost datapoint in Figure 5 in
# A. D. Barnosky, Megafauna biomass tradeoff as a driver of Quaternary and future extinctions. Proc. Natl. Acad. Sci. 105, 11543–11548 (2008).
# Extrated using: https://apps.automeris.io/wpd/
wet_and_wild <- 0.18 * 10^12 # kg wet biomass

# [kgC]
carbon <- wet_and_wild * wet_to_c

# kg to Pg
kg_to_Pg = 1e-12

# [PgC]
carbon * kg_to_Pg

