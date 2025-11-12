# Spatial heterogeneity in lake fish populations and the impacts of non-native predators

![image of a smallmouth bass](https://github.com/meairey/literate-potato/blob/main/Graphics/SMB_image.jpg?raw=true)

Patchy habitats create patchy responses of native fish communities to the removal of an invasive predator


Airey et al., in prep 

## Overview
This repository contains data and analyses from a smallmouth bass removal project that is managed by Cornell's Adirondack Fishery Research Program. 

Little Moose Lake and First Bisby Lake have been the site of an ongoing smallmouth bass removal program aimed at restoring native fish community structure. For over two decades, the fish communities have been monitored using boat electrofishing surveys, providing a unique long-term, spatially explicit dataset on population and community dynamics.

This project assesses how spatially heterogeneous the community’s recovery has been by combining:

* Long-term community datasets derived from electrofishing
* Modern habitat assessments across each lake.

# Repository structure

The `Analysis` folder contains the `.R` script for generating changepoints and regressions. The data that goes into this script is generated in the `Data` folder in the file `data_processing` script and uses functions as created in the `Function_Source_Files` folder. Graphics for the manuscript are kept in the `Figures_Tables` folder. Please see Detmer et al., in review for more information on sampling and history of the project.

# Requirements

Analyses use of a change point analysis through the package `ecp` and a zero-inflated regression through the package `pscl`. Additionally, habitat associations are assessed through a CCA analysis and the package `vegan`.

`install.packages(c("ggplot2", "tidyverse", "ecp", "pscl", "vegan", "gridExtra", "ggrepel", "mass", "lme4", "wesanderson", "broom", "emeans"))`

# Results Summary

* Juvenile overcompensation by a non-native predator is negatively associated with abundances of small-bodied fishes

* The larger, more heterogeneous lake supports greater spatial partitioning between natives and the juveniles of the non-native predator

* Changes in the community composition across sediment habitats were more stable through time, suggesting that these habitats may buffer natives from the cobble-prefering bass

Below is an example of the summary results of the change point analysis. 

![image of graph](https://github.com/meairey/crispy-bassoon/blob/main/Figures_Tables/Fig5_ChangepointLML.jpeg)

## Data availability

The data are included in the `.gitignore`. Please contact `ma2276@cornell.edu` with questions about data availability. We are happy to share cleaned data.




