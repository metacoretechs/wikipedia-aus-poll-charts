# wikipedia-aus-poll-charts
Political polling data and R code to generate charts for Wikipedia articles such as [Opinion polling for the next Australian federal election](https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Australian_federal_election).

## Installing R
The script is written in the R programming language. To generate the charts, you will need to have access to an R interpreter. I recommend you use the R integrated development environment RStudio.

1. Download and install [the latest version of R](https://cran.rstudio.com) for your operating system.
2. Download and install [RStudio](https://www.rstudio.com/products/rstudio/download/).

## Updating the polling data
1. I used [OpenRefine](http://openrefine.org) to clean up the Wikitable of polling data from [the "Next Australian federal election" Wikipedia article](https://en.wikipedia.org/wiki/Next_Australian_federal_election#Voting_intention), but if you download the file from this repository, you won't need to do this.
2. If polling22.25.csv hasn't been updated to the latest poll(s), you can add them yourself to the CSV file using a text editor or spreadsheet software like Excel.
3. The header row has the column headings \[Date,last_date,Firm,pv_lnp,pv_alp,pv_grn,pv_onp,pv_uap,pv_oth,tpp_lnp,tpp_alp\] which match the Wikipedia table, except I have inserted a new column "last_date", which is the final date where the poll was conducted over a date range. The script uses the last_date to plot the survey, so if you are adding a new poll, put the end date of the range in the second column. Date (the date range) and Firm (company doing the poll) are not used, but are included in case you wish to do further analysis using this data.
4. Polls by Essential Research use a methodology called "2PP+", where the percentage of undecided voters is included. Essential poll data is listed in a separate CSV file (essential_polling1922.csv) which includes a column for the undecided voter percentage. The script will then calculate an estimated primary vote and two-party preferred vote by distributing the undecided figures to the same proportions as the decided figures. This mimics the methodology of the other polls which exclude undecided voters and publish the breakdown of decided voters only.
5. There is a column of the survey's sample size, which is used in a separate script `polling-graph-weighted.r`. This script uses the sample size to weight the LOESS regression, and displays the dots of the scatterplot by size.

## Generating the charts
1. Open RStudio, and click on the Packages tab or go to Tools > Install Packages...
2. Install the ggplot2 package.
3. Open polling-graph.r in RStudio, and adjust the read.csv function to point to "polling2225.csv".
4. Select all the code in polling-graph.r and press the Run button.
5. The primary vote and two-party-preferred charts will generate in the Plots tab. You can export these to your desired size by pressing Export > Save as image... The PNG export in RStudio isn't great, and vector images are generally preferred on Wikimedia Commons, so export as a 1200 x 800 SVG image.
6. Upload the resulting SVG or PNG image files to Wikimedia Commons.
