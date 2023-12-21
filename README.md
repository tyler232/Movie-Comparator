# Movie-Comparator

<img width="1111" alt="Screenshot 2023-11-02 at 10 30 46 PM" src="https://github.com/tyler232/Movie-Comparator-App/assets/89470281/1d7ade0c-6091-4c9a-a6a7-c146956cf27d">


## Overview

Movie Comparator App is a R shiny app that allows users to compare the score between different movies across different rating platforms. This application provides a user interface to search for movies, view details about scores, and visualize the comparison through histogram.

The application is accessible [here](https://tye232.shinyapps.io/Final_project/).

## Features

- Search for movies by title and year.
- View detailed information about rating and votes between movies.
- Visualize the comparison.

## How to Use

1. Access the Movie Comparator web application [here](https://tye232.shinyapps.io/Final_project/).

2. Use the search bar to find movies by title, if a different movie that has the same name came out, try adding release years to the desired movie under title search bar.

3. To compare movies, click the "Compare" button on the left side.

4. The histogram of compared movies will be displayed side by side, allowing you to analyze their details.

## How to Run Locally

If you want to run this application locally or make change to the project, follow these steps:

1. Make Sure you have R and RStudio downloaded
2. Clone the repo
```bash
git clone https://github.com/tyler232/Movie-Comparator-App.git
```
3. Install Shiny in RStudio Terminal
```r
install.packages("shiny")
```
Or in PC Terminal directly
```bash
sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
```

## Acknowledgments
[The Open Movie Database(OMDb)](http://www.omdbapi.com) for providing the movie data through their API.

