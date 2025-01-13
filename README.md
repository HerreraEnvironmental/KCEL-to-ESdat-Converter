
# King County Environmental Lab (KCEL) to ESdat Converter

README last updated on 2024-08-21

Repository for scripts related to KCEL to ESdat tool.

------------------------------------------------------------------------

# KCEL to ESdat Convertor Tool

### Tool to convert KCEL EDDs to ESdat XML format

------------------------------------------------------------------------

## 🔧 Pull Requests

Pull requests are welcome. For major changes, please open an issue
first.

All functioning code is located on the main branch. Dev branches are to
be named <specific_issue_description>\_dev.

## 💬 Contributors + Contact Information

- [Nicholas Harris](https://github.com/nharris-HEC)
- [Nikki VandePutte](https://github.com/nvandeputte)

## Steps for Converting EDDs (desktop)
This tool is a work in progress. Currently the app only converts KCEL EDDs, but there are also scripts for Onsite+Amtest and Exact Scientific. Follow the steps below to use the scripts for the additional labs.

1. Copy CSV and PDF files into {repo}/data/{lab}/data_raw
1. Update the config.yaml file with project-specific ESdat details
1. Run the {repo}/ESdat-Convert-Tools/supporing-scipts/{lab}/ESdat_prep.R script
1. Look for your zipped up EDDs in the {repo}/zips/
