## Purpose
The first R code script creates two graphs, concerning death registrations and mortality rates.
This is for England and Wales, between 1900 and 2020.
The 2020 figures are provisional.

The second file produces graphs on age-standardised mortality rates.

The third file produces a graph showing a stacked bar graph with a straight line for the past average.

## Data source
The Office for National Statistics produced an ad hoc spreadsheet. I have adapted this spreadsheet.
- Office for National Statistics (ad hoc request): https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12735annualdeathsandmortalityrates1938to2020provisional
- Office for National Statistics (weekly report, for registrations ending 2nd April 2021):
- https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/bulletins/deathsregisteredweeklyinenglandandwalesprovisional/weekending2april2021

## Packages
For create a 'joined' graph, I use Thomas Lin Pederson's patchwork package.
- patchwork: https://github.com/thomasp85/patchwork

I also use Prof Matthew Kay's ggdist package:
- ggdist: https://mjskay.github.io/ggdist/

I use a function from Claus Wilke's ungeviz package:
- ungeviz: https://github.com/wilkelab/ungeviz

## R Markdown
There is an R Markdown page to view.
- ONS mortality analysis I (13th February 2021): https://rpubs.com/anthonybmasters/ons-mortality-rates
- ONS mortality analysis II (24th March 2021): https://rpubs.com/anthonybmasters/ons-mortality-rates-ii
- ONS weekly death registrations figure 3 (14th April 2021): https://rpubs.com/anthonybmasters/ons-weekly-death-reg-figure-3
