# CDC Tax Burden on Tobacco
This repository provides the necessary code and links to download and organize the CDC's Tax Burden on Tobacco dataset.

## Raw Data
The raw data is available directly from the Centers for Disease Control and Prevention (CDC): [Tax Burden on Tobacco](https://data.cdc.gov/Policy/The-Tax-Burden-on-Tobacco-1970-2018/7nwe-3aj9/data). There are also lots of built-in tools to work with these data within the website. For this repo, I downloaded the dataset as a simple csv file, so that is the starting point.

For our basic price graphs, we want to focus on *real* prices. So we supplement the Tobacco data with CPI data from the Bureau of Labor Statistics:[CPI, All Urban Consumers](https://www.bls.gov/cpi/data.htm). 

## Code
The csv file has lots of different variables per row. We want all of the variables to be columns, so the following code file will `tidy` the data appropriately [build-data.R](build-data.R). This code also imports the CPI data, reshapes as needed, and merges to the Tobacco data.

