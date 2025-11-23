library(tidyverse)
library(nflreadr) 

# read nfl data from previous project 




all_contracts = nflreadr::load_contracts()

# only modern contracts 
# distinguish between free agent signings and draft picks 
# then save results as seperate file 

