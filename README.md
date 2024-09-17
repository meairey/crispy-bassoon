![image of a smallmouth bass](https://github.com/meairey/literate-potato/blob/main/Graphics/SMB_image.jpg?raw=true)


# Patchy habitats create patchy responses of native fish communities to the removal of an invasive predator 
Airey et al., in prep 

## Study framework 
Little Moose Lake is the site of a long term smallmouth bass removal project. The fish community within this lake has been monitored for the last two decades. The goal of this analysis is to examine long-term trends in catch per unit effort (CPUE) and size of the fish community in response to that bass removal.

This is primarily conducted through the use of a change point analysis through the package `ecp` and a zero-inflated regression through the package `pscl`. Additionally, habitat associations are assessed through a CCA analysis and the package `vegan`.


Below is an example of the summary results of the changepoint analysis. 

![image of graph](https://github.com/meairey/crispy-bassoon/blob/main/Figures_Tables/Fig5_ChangepointLML.jpeg)

## Data availability

The data are included in the `.gitignore`. Please contact `ma2276@cornell.edu` with questions about data availability. We are happy to share cleaned data.

## Repository structure

The `Analysis` folder contains the `.R` script for generating changepoints and regressions. The data that goes into this script is generated in the `Data` folder in the file `data_processing` script and uses functions as created in the `Function_Source_Files` folder. Graphics for the manuscript are kept in the `Figures_Tables` folder. Please see Detmer et al., in review for more information on sampling and history of the project.


