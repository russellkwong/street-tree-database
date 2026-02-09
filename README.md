# A database of urban trees in the Eastern and Midwestern United States 

A cleaned and formatted database of ~7 million trees documented in municipal tree inventories in the Eastern and Midwestern US.

## Description

In the U.S., urban foresters and other tree professionals are responsible for city-owned trees, including from planting, to maintenance, and removal. To effectively manage these large numbers of city-owned trees, urban foresters generally use GIS software and databases to track individual trees and the work being done on them. The largest published collections of which we are aware contain 49 Californian cities in California, 23 cities across the US and Canada, and there are also collections of municipal inventories for Indiana, New York, Pennsylvania, Wisconsin, and Vermont. These urban tree databases, sometimes referred to as tree inventories, typically record tree species, size, and location. Other national-scale datasets of urban trees, such as the Urban Forest Inventory & Analysis program, only provides information at the plot scale and areis poorly suited for linking individual trees with spatially explicitgranular remote sensing observations.

Here, we compile municipal street tree inventories into a unified database of individual street trees across the Eastern and Midwestern United States. The 300 cities include 7.1 million trees and span a broad climatic (7.6°C - 25.2°C) and latitudinal gradient (25.5°N - 44.5°N). The resulting processed and ready-to-use database includes (at minimum) the species, stem diameter, and coordinates of urban trees. Information such as inventory date and tree condition are also included when available. This dataset will allow researchers to compare urban forest dynamics, phenology, and management strategies across the Eastern and Midwestern United States. It also addresses a main barrier to using remote sensing data at the individual tree level by providing coordinates of trees of known size and identity. 

## Getting Started

### Dependencies

Software to view and manipulate a large csv file (>700 MB):
* RDBMS (for viewing and manipulating data)
* IDEs (for data analysis and visualization - <i>R, Python, etc.</i>)
* GIS software (for spatial analysis)

<b>Note:</b> Spreadsheet applications such as MS Excel and Google Sheets may encounter issues reading and loading the entire dataset.

### Dataset

The dataset is available on Box along with all supporting files.  
[R4LIST (on Box)](https://cornell.app.box.com/file/2129327461712) ~700MB 
[R4LIST_sample (on Box)](https://cornell.app.box.com/file/2129350917535) ~35MB
[Entire Box folder with supporting files](https://cornell.box.com/s/hlhkk8w3swc0m5bns19rerm2cx5859x9)

## Supporting files

[city_metadata.csv](https://github.com/russellkwong/street-tree-database/blob/main/city_metadata.csv) - Contains inventory sources, lastest update dates, and total number of trees per city.  
[cityinv_cols.csv](https://github.com/russellkwong/street-tree-database/blob/main/cityinv_cols.csv) - Original column names for each inventory (mainly for R processing).

Processing scripts:  
[dataCleaning_main.R](https://github.com/russellkwong/street-tree-database/blob/main/dataCleaning_main.R) - Main processing script using functions from following files.  
[qualityControl_dbh.R](https://github.com/russellkwong/street-tree-database/blob/main/qualityControl_dbh.R) - Handles quality checks and processing for DBH and height.  
[qualityControl_spp.R](https://github.com/russellkwong/street-tree-database/blob/main/qualityControl_spp.R) - Handles quality checks and species matching.  
[qualityControl_extra.R](https://github.com/russellkwong/street-tree-database/blob/main/qualityControl_extra.R) - Handles quality checks and processing for additional info columns.  
[inventoryFormat.R](https://github.com/russellkwong/street-tree-database/blob/main/inventoryFormat.R) - City-level preprocessing after first download of city inventory. 

## Authors

Russell Kwong  
[rk584@cornell.edu](mailto:rk584@cornell.edu)

Dr. Daniel S.W. Katz  
[dankatz@cornell.edu](mailto:dankatz@cornell.edu)

## Known Issues

09 February
* Several inventories have city-wide issues with DBH processing and were dropped from the dataset. Notes: [city_dbh_check.csv](https://github.com/russellkwong/street-tree-database/blob/main/city_dbh_check.csv)
* Not all condition strings are properly formatted. 
* Municipality metadata must be updated with unique GNSI identifers and sources unhyperlinked. File: [city_metadata](https://github.com/russellkwong/street-tree-database/blob/main/municipality_info.csv)

02 February
* Several inventories have city-wide issues with Lat Long coordinates processing and were dropped from the dataset. \[FIXED 09 FEB\]

## License

## Acknowledgments
