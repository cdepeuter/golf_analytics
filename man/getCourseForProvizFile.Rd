% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proviz.R
\name{getCourseForProvizFile}
\alias{getCourseForProvizFile}
\title{The correspondig pro-viz file for a given course}
\usage{
getCourseForProvizFile(file)
}
\arguments{
\item{string}{input name of course}
}
\value{
string of file in /data/proviz directory for the corresponding file name.
}
\description{
This function finds the corresponding proviz course file in the data/proviz/ directory
There is a text file, exceptions.txt which manually corrects some errors
}

