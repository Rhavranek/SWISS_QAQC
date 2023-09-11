#Repo Description 

There is a lot going on in this repository, because it hosts all of the SWISS data that I (Rachel Havranek) produced in 2021 and 2022. In this repository there is an R markdown for the QA/QC of each individual SWISS unit (e.g. Toblerone, Rosti, Meringue). The R markdown file named "offset" has all of the plots and calculations I used to create the 'offset' correction described in Havranek et al., 2023, HESS. The R markdown file named "dryairtests" plots all of the data from dry air tests done, and makes the appropriate figures in Havranek et al., 2023. The R markdown file named "calibrations" is where I reduce the data for standards run both through the probes and standards run using the furnace in INSTAAR. Those calibrations are then put into "scripts/corrections.R" and used to correct the data into VSMOW/SLAP in other R markdown files. There is also an R markdown file dedicated to each of the 3 field sites where SWISS units were deployed (Oglala, Briggsdale, and Seibert). 



#Scripts Descriptions

"plotting_functions.R" comes from the Kopf lab at CU Boulder, and is primarily used to make uniform figures
"table_functions.R" also comes from the Kopf lab and is used to make publication tables
"Rachels_custom_plots.R" has functions that I wrote for plots that I would want to make over and over again (like d18O over a time period, investigating if my averaging period is stable, etc.)
"PicarroData_BreakUP.R" has functions that I wrote that automatically cut up a long time series (e.g. 24 hours) and group data by flask, and then can take differnt subsets of those groups to average the Picarro data
"Eriks_functions.R" are functions that Erik Oerter wrote for the IsoWagon (Oerter et al., 2016) that I have modelled some of my functions off of
"Corrections.R" are calculated corrections for moving data into VSMOW/SLAP and applying the offset correction described in Havranek et al., 2023. 



