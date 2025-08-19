# AgQuery-Refresh

A Shiny port of our simple indicator database lookup tool. AgQuery makes searching and filtering EPAR's LSMS-ISA summary statistics easier.

Initial Setup:

1. Running AgQuery requires R and R Studio (see setup instructions [here](https://posit.co/download/rstudio-desktop/))
2. All of the dependencies required to run the app are managed using [renv](https://rstudio.github.io/renv/). After downloading the repository, open the .Rproj file and install renv using `install.packages("renv")` if prompted, then run renv::hydrate() in the console. 
3. Open the app.R file and press the "Run" button that appears in the upper-right corner of the code pane.
4. AgQuery can also be server-hosted using Shiny Server or Docker.
5. DuckDB is currently used for improved performance, but the app can be easily modified to use the csv dataset instead.
