# Submission 2:

## Resubmission
This is a resubmission. In this version we have addressed the following issues:

* "Please always write package names, software names and API (application programming interface) names in single quotes in title and description. e.g: --> 'stars' ":  
  - This was checked for all function titles and the DESCRIPTION. Changes were made in the DESCRIPTION and tidyverse.Rd.
  
* "Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)
Missing Rd-tags:
plot.sftime.Rd: \value
st_as_sftime.Rd: \value
transform.sftime.Rd: \value"  
  - The \value tag was added to plot.sftime.Rd.
  - The \value tag was added to st_as_sftime.Rd.
  - The \value tag was added to transform.sftime.Rd
  
  
* Additional changes:
  - The documentation for several functions was improved.

## R CMD check results



# Submission 1:

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
