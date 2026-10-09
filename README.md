# Project name: Using semi-structured citizen science data to model the effect of land-use composition on bird occupancy trends in Germany
In our study, we analysed spatial and temporal occupancy patterns across 91% of Germany’s terrestrial land 
at regional resolution, using a 15-year dataset of garden bird populations. By integrating citizen science data 
with land-use composition, the study analyses how land-use composition differentially impacts biogeographic
patterns in bird occupancy across different trait groups (habitat and diet). 
Our results highlight that even common, settlement-adapted bird species are declining particularly 
in urban-dominated regions, with granivorous and urban-affiliated species showing the most negative trends. 
The study’s methodological framework offers a scalable method for analysing semi-structured datasets
with coarse spatial resolution (no precise GPS data) to study biodiversity patterns alongside biogeographic 
processes with global applicability across taxa and regions. 

# Used R environment:
This project uses `renv` to ensure that all package versions used are reproducible. 
When you open the project in RStudio, simply run `renv::restore()` in the console to 
automatically install all dependencies.

# read Data:
Please note that the main data file (`260730_bird_prop_landuse_ZIP3_final.csv.gz`) has been compressed to save storage space. 

# License and Data use
# Code
The R scripts in this repository are licensed under the GNU General Public License v3.0 (GPLv3). You are free to use, modify, and share the code.
# Data
The observation data provided in the `/Bird_data` folder are **the property of NABU**. NABU (Naturschutzbund Deutschland e. V.) Bundesverband‚ Stunde der Gartenvögel‘; Data of the years 2006-2021
- The data were requested for this project and are available in **aggregated form**.
- They are published with the kind permission of NABU.
- Further use or commercial exploitation of this data by third parties is not permitted without the explicit consent of NABU.
