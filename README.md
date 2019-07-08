---
output: 
  html_document: 
    keep_md: yes
---
# Biotic Explorer 
**Shiny app to open, examine and manipulate NMD Biotic xml files used by the Institute of Marine Research Norway**

This is a prototype of the **Biotic Explorer** [Shiny](https://shiny.rstudio.com/) app allowing examination and manipulation of the Norwegian Maritime Data-center (NMD) standard xml files, which are used within the Institute of Marine Research database. While the application is developed by the Institute of Marine Research, **this is not the official version** of the application and is bound to contain bugs and other errors. 

## Installation

Running the app for the first time **automatically installs** packages used by the app.

### First time installation

The app requires [R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/). Install these software on your computer following the instructions on the respective webpages. Open RStudio and install the [Shiny](https://shiny.rstudio.com/) package:




```r
install.packages("shiny")
```

### Running the app directly from GitHub

Write:


```r
runGitHub("BioticExplorer", "MikkoVihtakari")
```

### Running the app from your hard drive

Click "Clone or download" -> "Download ZIP". Find the zip file (typically in your Downloads folder) and extract it to a desired location. Open the app.R file in RStudio and [click "Run app"](https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/).

## Usage

- 1) **Upload data:** Click 'Browse..' and select an xml file from your computer. An overview of data and sampling station locations will be shown under. You can use the available options to remove data that are not relevant.
- 2) **Filter data:** Use the 'Filter data by' options to select data you want to keep. Click the 'Subset' button once you are ready and see how the overview will change based on the information you selected.
- 3) You can examine the station and catch data by clicking the 'Stations & catches' tab. Use the 'Overview' sub-tab for a graphical overview or the 'Examine' tab for a tabular overview, which you can filter and search as you wish, but note that filtering here does not influence the returned data.
- 4) Similarly, an overview of individual measured fish is given under 'Individuals & ages' tab.
- 5) 'Mission data' through 'Age data' tabs give a tabular overview of each data type in the NMD xml Biotic file.
- 6) **Download** filtered data using the 'Download' tab. Select the format you want to download in (R, csv or Excel). If you select multiple data types (station vs individual), note that the csv format will be returned as a zip file.

## Contributions and contact information

Any contributions to the app are more than welcome. Please contact the app creator Mikko Vihtakari (<mikko.vihtakari@hi.no>) to discuss your ideas on improving the app.

## Dependencies

Running the app automatically installs following packages:

- [shinyFiles](https://cran.r-project.org/web/packages/shinyFiles/index.html). Used to up- and download files.  
- [shinydashboard](https://cran.r-project.org/web/packages/shinydashboard/index.html). Used for the dashboard.
- [DT](https://cran.r-project.org/web/packages/DT/index.html). Used for data tables.
- [tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html). Used for data manipulation and plots.
- [RNMDAPI](https://github.com/REDUS-IMR/RNMDAPI). Used to read NMD .xml files.
- [devtools](https://cran.r-project.org/web/packages/devtools/index.html). Used to download the RNMDAPI package.
- [leaflet](https://cran.r-project.org/web/packages/leaflet/index.html). Used for interactive maps.
- [openxlsx](https://cran.r-project.org/web/packages/openxlsx/index.html). Used to write MS Excel files. 

## News

2019-07-08 Uploaded the first alpha version. The app works, but does not contain all features yet. 
