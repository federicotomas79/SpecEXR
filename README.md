
# SpecEXR

<!-- badges: start -->
<!-- badges: end -->

The SpectrEXR is a shiny app for single tree multi-spectral extraction This is a shiny app specially used to extract spectral information of a single tree, and I also provide a sample to show what the data looks like after extraction.
First of all, you need point cloud data with precise positioning information, which is used to segment each individual plant in a large area of forest land. In addition, you need to have multi-spectral or hyperspectral information of this forest land.


## Installation

You can install the development version of SpecEXR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Yanjie-Li/SpecEXR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SpecEXR)
## basic example code
SpecexR_app()

```
the first page is "Segmentation", it is an visualization  example that the crown spectral of every single tree on one  plantation site has been extracted. the page is showing like this:
![Screen](/images/segeme2.gif)

the second page is "VIs graphs generation", it is to calculation the VIs from the images based on the five bands raster images, including red, green, blue, rededge and near-infrared raster tif images. 
The workflow for the VIs calculation is shown in the flowing graph:
![Screenshot](/images/VIs.png)



The page is showing like this:
![Screenshot](/images/figure2.png)

the third page is "Tree identification and spectra extraction", it is is the core function of this app, The workflow for how to use this page can be found in the flowing graph:
![Screenshot](/images/treese.png)




