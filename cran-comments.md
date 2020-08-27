## Test environments
* local Windows 10, R 4.0.2
* local Ubuntu 18.04. R 4.0.2
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
I have also run R CMD check on downstream dependencies. There is no downstream dependency.
