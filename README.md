# 4SPAIN

### The 4SPAIN dataset

4SPAIN are seasonal forecasts of meteorological drought from a probabilistic approach in the Iberian Peninsula for several months in advance. It has been built by applying an ensemble based streamflow prediction system (ESP, an ensemble based on the reordering of historical data) using the ERA5 data as initial conditions. 
<br/>

4SPAIN is available from https://github.com/MTAV26/4SPAIN/tree/main/data from January 1979 to the last month. Files were created in NetCDF format following the Climate metadata standards. 4SPAIN provides information in a variety of ways:
<br/>
- SPI values. The 4SPAIN provides the values of the SPI index. This could be of use for user that need to quantify the drought conditions. 
<br/>
- SPI spread. The spread of 4SPAIN gives an estimation of the uncertainties of the observed SPI index. 
<br/>
- SPI warning level. This map is designed to allow users to distinguish between high drought severity with high probability and low severity and low probability. That is, the colours indicate a combination of probability and severity as indicated in the table below: 
<br/>
<br/>
<p align="center">
  <img src="https://github.com/MTAV26/4SPAIN/blob/main/drought_matrix_levels.png" width="450" title="hover text">
</p>
<br/>
For example, yellow could reflect either a high probability of abnormally dry conditions or a low probability of a severe drought. This approach is based on the 
guidelines for disaster management of the European Commission (EC 2010).
- SPI probability of drought level. This map shows the full probability of having (at least) moderate drought, thus giving a more detailed illustration of the 4SPAIN uncertainty and could be useful for the most experienced users.


### Input data

The following reference shows the data applied as predictors of 4SPAIN. 

| Dataset  | Source |
| :------------ |:---------------|

| ERA5  | https://www.ecmwf.int/en/forecasts/datasets/archive-datasets/reanalysis-datasets/era5      |
<br/>
<br/>

### Acknowledgments
A.H-M thanks his predoctoral contract FPU18/00824 to the Ministerio de Ciencia, Innovación y Universidades of Spain. M.T. acknowledges funding by the Spanish Ministry of Science, Innovation and Universities Ramón y Cajal Grant Reference RYC2019-027115-I. 

### Source Code
Full source code is available from the github repository https://github.com/MTAV26/4SPAIN.

### Citation
add citation

### References
EC (European Commission). (2010). Risk assessment and mapping guidelines for disaster management. Commission Staff Working Paper, SEC (2010) 1626 final, Brussels.
available at: https://ec.europa.eu/echo/files/about/COMM\_PDF\_SEC\_2010\_1626\_F\_staff\_working\_document\_en.pdf


<!---


* <a href="http://cran.r-project.org/web/packages/shiny" target="_blank_">shiny</a>: Chang, W., Cheng J., Allaire, J.J., Xie, Y. & McPherson, J. (2013). shiny: Web Application Framework for R. R package version 0.11.1
* <a href="http://cran.r-project.org/web/packages/shinydashboard" target="_blank_">shinydashboard</a>:Chang, W. (2015). shinydashboard: Create Dashboards with Shiny. R package version 0.5.1
-->
