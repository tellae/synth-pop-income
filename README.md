# Code for the paper "Methodology for Adding a Variable to a Synthetic Population from Aggregate Data: Example of Adding the Income Variable"

## Datasets and R scripts

We provided two datasets and four R scripts.

1) The dataset "synth_pop_original.feather" represents the synthetic household population for the city of nantes.

2) The dataset "deciles_filosofi.feather" represents the distribution of income deciles for the city of nantes.

These data are provided by the French National Institute of Statistics and Economic Studies (INSEE) and contain a large number of variables. We only retain the elements (households and variables) relevant to
our case study. For more information about the data, please refer to the section Raw data.

3) The R script "script_1.R"  deals with step 1 of the "problem solving heuristic" section (subsection 4.13)

4) The R script "script_2.R"  deals with step 2 of the "problem solving heuristic" section (subsection 4.13)

5) The R script "script_3.R"  deals with step 3 of the "problem solving heuristic" section (subsection 4.15)

6) The R script "script_4.R"  deals with the post-processing step and the validation of the results

Scripts must be run in this order

## Raw data

Raw data are provided by the French National Institute of Statistics and Economic Studies (INSEE) and freely available.

The census database used is available via the following link: https://www.insee.fr/fr/statistiques/3625223?sommaire=3558417 (consulted on November 27, 2020). Data were collected from 2013 to 2017 and adjusted to the reference year of 2015. This database (named "Individus localisés au canton-ou-ville - Zone C") is a sample of individuals and households. We kept elements of the city of Nantes (Variable "COMMUNE" equal to 44109) and then used variable "IPONDI" to weight the sample of households in order to get a synthetic population of 157,647 households (dataset "synth_pop_original.feather").

FiLoSoFi database is available via the following link : https://www.insee.fr/fr/statistiques/3560118 (consulted on November 30, 2020). We used the database named "Base niveau communes en 2015 - y compris arrondissements municipaux", spreadsheet "FILO_DISP_COM.xls". We selected the city of Nantes.
