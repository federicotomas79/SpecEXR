
# SpecEXR
![Screen](/images/22.png)

<!-- badges: start -->
<!-- badges: end -->

The SpecEXR is a shiny app for single tree multispectral extraction. This shiny app is especially used to extract spectral information of a single tree, and I also provide a sample to show what the data looks like after extraction.
First of all, point cloud data is needed with precise positioning information, which is used to segment each individual tree in a large area of forest land. In addition, multispectral or hyperspectral information is needed.


## Installation

You can install the development version of SpecEXR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Yanjie-Li/SpecEXR")
```

Note:`rtools` and "EBImage"  and other packages are required, `rtools` should be installed manually from: [rtools](https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html).  
 
 
# The source package

If there is something wrong and cannot install the shiny app from github, please try to download the source packages and install it directly from R or Rstudio from Archive files: [package](/source-package/SpecEXR_1.0.tar.gz)




## Example

This is a basic example which shows how to solve a common problem:

``` r
library(SpecEXR)
## basic example code
SpecexR_app()

```
The first page is "Segmentation". It is a visualization example where the crown spectral of single tree in one plantation site has been extracted. The example page is showing like this:

![Screen](/images/segeme2.gif)

The second page is "VIs graphs generation". It is used to calculate the VIs from the images based on five bands raster images, such as red, green, blue, rededge and near-infrared raster .tif images. 
The workflow for the VIs calculation is shown in the flowing graph:

![Screenshot](/images/VIs.png)


The example page is showing like this:

![Screenshot](/images/figure2.png)

The third page is "Tree identification and spectra extraction". This is the core function of this app. The workflow for how to use this page can be found in the flowing graph:

![Screenshot](/images/treese.png)

The example page is showing like this:

![Screenshot](/images/figr23.png)

## Data for the exercise

Please download the example data from link below:[example data](https://www.dropbox.com/sh/dncqmm0eh7ek7sw/AADgg3bgyHGz5HWa-I9wLQxra?dl=0)
or [example2](https://ln5.sync.com/dl/d6899c6f0/3g32725x-b85yuvm3-ba68kfre-jewun6fk)

