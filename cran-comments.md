## Updated version
This version fixes several bugs. There is no change in the packages' output.

## Test environments
* local Windows 10, R 4.0.2
* local Ubuntu 18.04. R 3.6.2
* local MacOS Mojave 10.14.5 R 3.6.1
* r-hub.io Ubuntu Linux 16.04, R-release
* r-hub.io Fedora Linux, R-dev

## R CMD check results
There were no ERRORs or WARNINGs. 

** There is one note as follow.
"> checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    'res.html'"

The 'res.html' file is generated from running the example code. By default, the results of the analysis are summarized in the res.html in html to format the results into a direct usable format. This can be disable by setting "HTML_report" to FALSE.

## Downstream dependencies
This is the first submission.
