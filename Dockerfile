# Use an R base image
FROM rocker/r-ver:4.3.1

# Create Directory for image
WORKDIR C:Users\hadsa\OneDrive\Documents\GitHub\CFB-ML-App

# Install necessary R packages
RUN R -e "install.packages('tidyverse',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('cfbfastR',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('oddsapiR',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('janitor',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('stringdist',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('xgboost',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidymodels',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shiny',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('snakecase',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('bslib',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('httr2',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('remotes',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('caret',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('Kazink36/cfbplotR')"

COPY . .

# Specify the command to run when the container starts
CMD R -e "source('00 runner.R')"
