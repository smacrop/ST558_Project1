# Code to render the README.md file -- Stephen Macropoulos

library(rmarkdown)
rmarkdown::render("C:/Users/Owner/OneDrive/Documents/ST501/ST558_Project1/README.Rmd",
                  output_format = "github_document",
                  output_dir = "C:/Users/Owner/OneDrive/Documents/ST501/ST558_Project1/Vignette/")