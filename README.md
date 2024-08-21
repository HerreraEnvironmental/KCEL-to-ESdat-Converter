
King County Environmental Lab (KCEL) to ESdat Converter
================
<Nicholas Harris> (Derived from Nikki VandePutte's White Lake Tool)
2024-08-21

Repository for scripts related to KCEL to ESdat tool.

------------------------------------------------------------------------------

## ![](readme_figures/Herrera_lockup_4c.png)


## Table of Contents

- [Project Description](#Project-Description) *REQUIRED*
- [Location in Herrera Database](#Location-in-Herrera-Database)
  *REQUIRED*
- [Requirements and Dependencies](#Requirements-and-Dependencies)
- [Installation and Usage](#Installation-and-Usage)
  - [Layout of directory and data](#Layout-of-directory-and-data)
  - [Detailed description of data and
    analysis](#Detailed-description-of-data-and-analysis)
  - [Metadata](#Metadata) *REQUIRED*
  - [Data discrepancies](#Data-discrepancies)
- [Pull Requests](#Pull-Requests)
- [Contributors and Contact
  Information](#Contributors-and-Contact-Information) *REQUIRED*

------------------------------------------------------------------------

# Project <XX-XXXXX> - Task <XXX>

### <Brief Project Title>

**SharePoint Site:** https://herrerainc.sharepoint.com/teams/24-08319-000-InternalDocs

------------------------------------------------------------------------

## Project Description

*\<Be as succinct as possible in this section.\>*

This analysis aims to…resulting in…

## :droplet: Location in Herrera Database

*\<Emojis in title headings are optional, but fun!\>*

The original, unmodified data used in this project is located in
*\<ensure you clearly label your raw, unmodified data.\>* That folder is
backed up to *\<https://herrerainc.sharepoint.com/teams/24-08319-000-InternalDocs/Shared%20Documents/Forms/AllItems.aspx?id=%2Fteams%2F24%2D08319%2D000%2DInternalDocs%2FShared%20Documents%2FInternal%20Docs%2FProject%2DFiles%2FMonitoringData&viewid=9105475c%2D5f40%2D4678%2Dbb88%2D4ed3e4d5e667\>*

If you do not have access to the data, please contact the emails listed
at the bottom of the repository.

## 📦 Requirements and Dependencies

Below is a list of packages and external softwares that this project
utilizes.

*\<Optional: Include a “typical imports” list.\>*

| Name                              | Description                                 |
|:----------------------------------|:--------------------------------------------|
| [`R`](https://www.r-project.org/) | Programming language used for this project. |

## :computer: Installation and Usage

*\<Ensure any installation specifics are listed here: versions,
environments, etc.\>*

In order to run this script and recreate the analysis, you will need to
have R and Rstudio installed on your computer. All the data produced by
this analysis can be found in the data_secondary/ folder, while all
figures can be found in the figures/ directory.

### :arrows_counterclockwise: Layout of directory and data

*\<Include a brief repository layout here. Make sure someone unfamiliar
with the project can see how the directory is organized and how the
analysis runs.\>*

This repository is organized into a main Complete.Rmd markdown script,
which produces the results from scratch when run in its entirety. The
Complete script references folders of raw data (data_raw/), and produces
results data that has been modified or created by the analysis
(data_secondary/). All analysis scripts are contained in the scripts/
directory, which also contains the src/ sub directory. The src/ sub
directory contains package loading scripts and scripts that produce or
modify data used throughout the analysis.

The data_raw/ folder is **READ ONLY** and should never be modified or
deleted.

### :heavy_check_mark: Detailed description of data and analysis

The raw data consists of *\<Type of data, amount of data, geographic
location, etc.\>*

The profiles are in *\<data format, import steps, etc.\>*

The naming convention is *\<describe how files are named and what any
abbreviations mean.\>*

*\<Here, include more detailed steps of the analysis, possibly in a
list. Step through the general process from beginning to end. Remember,
this might be YOU reading this in a few years, reminding yourself what
you did!\>*

*\<Include any additional external data that is relevant: websites,
databases, etc.\>*

### :information_source: Metadata

*\<This section is **required**, or this information must be contained
somewhere clearly in the repository.\>*

The vertical datum is… The horizontal datum is… All of the units are…

### :exclamation: Data discrepancies

*\<Include helpful tips about data quality or issues here. Did things
have to be renamed? Were some files deleted? Just provide a general
overview of quality. \>*

---------------------------------------------------------------------------

## 🔧 Pull Requests

Pull requests are welcome. For major changes, please open an issue
first.

All functioning code is located on the main branch. Dev branches are to
be named <specific_issue_description>\_dev.

## 💬 Contributors + Contact Information

- [Nicholas Harris](https://github.com/nharris-1927)
- [Nikki VandePutte](https://github.com/nvandeputte)
