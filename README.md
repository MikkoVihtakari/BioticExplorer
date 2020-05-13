
# Biotic Explorer

**A Shiny app to explore Biotic data within the Institute of Marine
Research Norway (IMR) database. Version 0.5.3 (alpha), 2020-05-13.**

This is the developmental version of the **Biotic Explorer**
[Shiny](https://shiny.rstudio.com/) app intended for examination and
manipulation of the Norwegian Maritime Data Center (NMD) standard Biotic
xml files as well as the IMR’s Biotic database (see the [User guide
section](#usage)). The application has been developed by the Stox team
at the IMR.

<!-- The official, stable, version of the app can be found from their [GitHub site](https://github.com/StoXProject).  -->

The app can be run on a desktop (i.e. your computer) for local files and
on a [server](http://astarte.imr.no/shiny) for both local files and the
entire IMR Biotic database.

## The server version

A server version of [Biotic Explorer is located on IMR
servers](http://astarte.imr.no/shiny/) and can be run through any modern
web browser given that **you access it inside the firewall of the
institute** (i.e. in the institute intranet or through a VPN).

## Installation of the desktop version

A desktop version of the app can be installed on any modern computer.
The app requires [R](https://www.r-project.org/) and
[RStudio](https://www.rstudio.com/). Install these software on your
computer following the instructions on the respective webpages. Open
RStudio and install the [Shiny](https://shiny.rstudio.com/) package:

``` r
install.packages("shiny")
```

Biotic Explorer uses the *RstoxData* package, which currently is only
available on
[GitHub](https://github.com/StoXProject/RstoxData/releases). On Unix
computers and on Windows machines with a working C++ compiler
([Rtools](https://cran.r-project.org/bin/windows/Rtools/)), you can
install the package directly from GitHub using the devtools package
(`devtools::install_github("StoXProject/RstoxData")`).

On computers that return errors when trying to run the Rtools through
RStudio (most institutional Windows machines), **install the binary
directly from
[here](https://github.com/StoXProject/RstoxData/releases)**. Download
the newest RstoxData zip file, click the “Packages” tab -\> “Install”
-\> “Install from:” “Package Archive File” -\> “Install”. If the
installer does not complain, the package is installed correctly.

As long as your C++ compiler or the RstoxData package work, running the
app for the first time **automatically installs and loads** packages
used by the app.

### Running the app directly from GitHub

Once you have the RstoxData package installed, you can run the app.
Write:

``` r
library(shiny)
shiny::runGitHub("BioticExplorer", "MikkoVihtakari")
```

### Running the app from your hard drive

Click “Clone or download” -\> “Download ZIP”. Find the zip file
(typically in your Downloads folder) and extract it to a desired
location. Open the app.R file in RStudio and [click “Run
app”](https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/).

## Usage

### Read data

#### Read NMD Biotic xml files

#### Download data from the database

### Examine data

#### Cruise overview data

#### Station data

#### Individual data

### Export

#### Export data

#### Export figures

1.  **Upload data:** Click ‘Browse..’ and select an xml file from your
    computer. An overview of data and sampling station locations will be
    shown under. You can use the available options to remove data that
    are not relevant.
2.  **Filter data:** Use the ‘Filter data by’ options to select data you
    want to keep. Click the ‘Subset’ button once you are ready and see
    how the overview will change based on the information you selected.
3.  You can examine the station and catch data by clicking the ‘Stations
    & catches’ tab. Use the ‘Overview’ sub-tab for a graphical overview
    or the ‘Examine’ tab for a tabular overview, which you can filter
    and search as you wish, but note that filtering here does not
    influence the returned data.
4.  Similarly, an overview of individual measured fish is given under
    ‘Individuals & ages’ tab.
5.  ‘Mission data’ through ‘Age data’ tabs give a tabular overview of
    each data type in the NMD xml Biotic file.
6.  **Download** filtered data using the ‘Download’ tab. Select the
    format you want to download in (R, csv or Excel). If you select
    multiple data types, note that the csv format will be returned as a
    zip file.

## Contributions and contact information

Any contributions to the app are more than welcome. Please contact the
app creator Mikko Vihtakari (<mikko.vihtakari@hi.no>) to discuss your
ideas on improving the app.

## Dependencies

Running the app automatically installs following packages:

  - [shiny](). The app has been written for Shiny.
  - [shinyFiles](https://cran.r-project.org/web/packages/shinyFiles/index.html).
    Used to up- and download files.  
  - [shinydashboard](https://cran.r-project.org/web/packages/shinydashboard/index.html).
    Used for the dashboard.
  - [DT](https://cran.r-project.org/web/packages/%20DT/index.html). Used
    for data tables.
  - [data.table](). Used to make data processing quicker.
  - [dtplyr](). Used for dplyr syntax applied to data.tables as well as
    quick database management.
  - [tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html).
    Used for data manipulation and plots.
  - [RstoxData](https://github.com/StoXProject/RstoxData). Used to read
    NMD .xml files.
  - [devtools](https://cran.r-project.org/web/packages/devtools/index.html).
    Used to download the RNMDAPI package.
  - [leaflet](https://cran.r-project.org/web/packages/leaflet/index.html).
    Used for interactive maps.
  - [leaflet.minicharts]()
  - [plotly]()
  - [openxlsx](https://cran.r-project.org/web/packages/openxlsx/index.html).
    Used to write MS Excel files.
  - [scales]()
  - [fishmethods]()
  - [viridis]()
  - [mapview]()
  - [DBI]()
  - [MonetDBLite]()

## News

2020-05-13 Added complete database support. All features visioned at the
beginning of the development have now been incorporated. Making the
features to work as planned, bug-fixing and polish remain.

2020-01-22 An update preparing for beta-release. Many new added
features. Unstable, full of bugs and undocumented.

2019-07-11 Fixed a number of Windows related problems. The app should
(hopefully) work now on most institutional machines.

2019-07-08 Uploaded the first alpha version. The app works, but does not
contain all features yet. This version is meant for internal testing.
