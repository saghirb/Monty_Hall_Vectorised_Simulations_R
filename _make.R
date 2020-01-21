# Making the Monty_Hall_Vectorised_Simulations_R repo

# Setup
library(here)

# Render the GitHub
rmarkdown::render(here("README.Rmd"), output_dir = here(), clean = TRUE)

# Extract R code from README.Rmd
knitr::purl(input = here("README.Rmd"),
            output = here("Monty_Hall_Vectorised_Simulations_R.R"),
            documentation = 1L)
