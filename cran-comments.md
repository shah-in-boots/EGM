## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Re-submission

In response to the helpful feedback from the CRAN team, I've additionally made the following changes.

1. In the DESCRIPTION file, software/package names require single quotation: I have added the appropriate single-quotes around these names. 

2. The DESCRIPTION file, under the "description" element, has inappropriate/too many white spaces: The formatting was reviewed and adjusted to avoid excess white space at linebreaks.

3. The exported methods and return tags were missing in the documentation for several functions ("colors", "header_table()", "signal_table()", "wfdb_paths()"): I have updated the documentation to explain the return values, including class and interpretation for the missing files. I apologize for the oversight.
