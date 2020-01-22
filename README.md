# Biotic Explorer 
**A Shiny app to open, examine and manipulate NMD Biotic xml files used by the Institute of Marine Research Norway**

This is a prototype of the **Biotic Explorer** [Shiny](https://shiny.rstudio.com/) app allowing examination and manipulation of the Norwegian Maritime Data-center (NMD) standard xml files, which are used within the Institute of Marine Research database. While the application is developed by the Institute of Marine Research, **this is not the official version** of the application and is bound to contain bugs and other errors. 

## Installation

The app requires [R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/). Install these software on your computer following the instructions on the respective webpages. Open RStudio and install the [Shiny](https://shiny.rstudio.com/) package:




```r
install.packages("shiny")
```

Biotic Explorer uses the new RstoxData package, which currently is only available on [GitHub](https://github.com/StoXProject/RstoxData/releases). On Unix computers and on Windows machines with a working C++ compiler ([Rtools](https://cran.r-project.org/bin/windows/Rtools/)), you can install the package directly from GitHub using the devtools package (`devtools::install_github("StoXProject/RstoxData")`). 

On computers that return errors when trying to run the Rtools through RStudio (most institutional Windows machines), **install the binary directly from [here](https://github.com/StoXProject/RstoxData/releases)**. Download the newest RstoxData zip file, click the "Packages" tab -> "Install" -> "Install from:" "Package Archive File" -> "Install". If the installer does not complain, the package is installed correctly.

As long as your C++ compiler or the RstoxData package work, running the app for the first time **automatically installs and loads** packages used by the app. 

### Running the app directly from GitHub

Once you have the RstoxData package installed, you can run the app. Write:


```r
library(shiny)
shiny::runGitHub("BioticExplorer", "MikkoVihtakari")
```

### Running the app from your hard drive

Click "Clone or download" -> "Download ZIP". Find the zip file (typically in your Downloads folder) and extract it to a desired location. Open the app.R file in RStudio and [click "Run app"](https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/).

### Troubles getting R/RStudio/devtools to recognize Rtools on Windows?

This is a common problem on IMR Windows computers and can now be avoided by installing a binary version of the RstoxData package (see above). If you want to get the C++ compiler to run on your machine, you need to install Rtools (only under Windows - the compiler is already installed on Unix machines). When you install Rtools, make sure to tick the ["edit the system PATH" option](http://stat545.com/packages01_system-prep.html). Restart RStudio, load devtools (`library(devtools)`) and see what `find_rtools()` returns (should be TRUE). There are numerous reported issues when trying to get the compiler to work. These issues are often caused because the development software for R (Rtools and pkgbuild) drag behind the R releases on Windows. See [here](https://github.com/r-lib/devtools/issues/1772), [here](https://stackoverflow.com/questions/51830648/rstudio-not-finding-rtools), and [here](https://stackoverflow.com/questions/33103203/rtools-is-not-being-detected-from-rstudio). Also setting `options(buildtools.check = NULL)` in RStudio sometimes works. 

## Usage

1. **Upload data:** Click 'Browse..' and select an xml file from your computer. An overview of data and sampling station locations will be shown under. You can use the available options to remove data that are not relevant.
2. **Filter data:** Use the 'Filter data by' options to select data you want to keep. Click the 'Subset' button once you are ready and see how the overview will change based on the information you selected.
3. You can examine the station and catch data by clicking the 'Stations & catches' tab. Use the 'Overview' sub-tab for a graphical overview or the 'Examine' tab for a tabular overview, which you can filter and search as you wish, but note that filtering here does not influence the returned data.
4. Similarly, an overview of individual measured fish is given under 'Individuals & ages' tab.
5. 'Mission data' through 'Age data' tabs give a tabular overview of each data type in the NMD xml Biotic file.
6. **Download** filtered data using the 'Download' tab. Select the format you want to download in (R, csv or Excel). If you select multiple data types, note that the csv format will be returned as a zip file.

## Contributions and contact information

Any contributions to the app are more than welcome. Please contact the app creator Mikko Vihtakari (<mikko.vihtakari@hi.no>) to discuss your ideas on improving the app.

## Dependencies

Running the app automatically installs following packages:

- [shinyFiles](https://cran.r-project.org/web/packages/shinyFiles/index.html). Used to up- and download files.  
- [shinydashboard](https://cran.r-project.org/web/packages/shinydashboard/index.html). Used for the dashboard.
- [DT](https://cran.r-project.org/web/packages/DT/index.html). Used for data tables.
- [tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html). Used for data manipulation and plots.
- [RstoxData](https://github.com/StoXProject/RstoxData). Used to read NMD .xml files.
- [devtools](https://cran.r-project.org/web/packages/devtools/index.html). Used to download the RNMDAPI package.
- [leaflet](https://cran.r-project.org/web/packages/leaflet/index.html). Used for interactive maps.
- [openxlsx](https://cran.r-project.org/web/packages/openxlsx/index.html). Used to write MS Excel files. 

## News

2020-01-22 An update preparing for beta-release. Many new added features. Unstable, full of bugs and undocumented.

2019-07-11 Fixed a number of Windows related problems. The app should (hopefully) work now on most institutional machines. 

2019-07-08 Uploaded the first alpha version. The app works, but does not contain all features yet. This version is meant for internal testing. 
