#' Do the extraction from raster images using las point data
#'
#'
#'multi_rasl is a function that used in the shiny app for data extraction,
#'it first use the point cloud data to do tree detection and tree crown polygon, and then
#'use this polygon to apply on the multispectal image to extract the multispecral
#'of each tree and then save as a data list
#'
#'
#' @param las_list a list of point cloud data
#' @param dsf_list a list of raster image data, mainly are tif image that read as raster
#' @param kwsindice the parameter for find tree
#'
#' @return a data list
#' @export
#'
#' @examples none
#'
#'
#'
multi_rasl <- function(las_list,dsf_list,kwsindice){
  lapply(las_list, function(x){
    expr <- tryCatch({
      library("lidR")
      library("rgdal")
      library(sfheaders)
      library(stars)
      library(raster)
      library(tidyverse)
      library(sf)
      library(data.table)
      message( paste0(names(x@header@VLR$Extra_Bytes$`Extra Bytes Description`)))
      tictoc:: tic("processing las file")
      las = classify_ground(x,csf(cloth_resolution = 0.5, class_threshold = 0.15, rigidness = 1), last_returns = FALSE)
      # pologon1 <- readRDS('e:/OneDrive - Business/宋钊颖/pologon1.rds')
      # proj4string(pologon1) <- CRS("+init=epsg:32650")
      subset = las#lidR::clip_roi(las,pologon1)
      dtm <- grid_terrain(subset, res = 0.5, algorithm = knnidw(k=5,p = 0.5), use_class = c(1L, 2L),keep_lowest = F)
      nlas_dtm  <- subset - dtm
      chm    <- grid_canopy(nlas_dtm , res =0.5, p2r())
      ttops  <- find_trees(nlas_dtm , lmf(ws= kwsindice , hmin=2.6, shape = "circular"))
      algo   <- dalponte2016(chm, ttops )
      lass   <- segment_trees(nlas_dtm, algo, uniqueness ='incremental')
      crown_polo  <- delineate_crowns(lass, func = .stdtreemetrics)
      directions_longlat <- spTransform(crown_polo,sp:: CRS("+proj=longlat +datum=WGS84 +no_defs"))
      sf  <- st_as_sf(directions_longlat)
      sf33 <- as.data.frame(crown_polo)
      sf33 <- sf33%>%dplyr:: select(treeID,ZTOP,convhull_area)%>% as.data.frame()
      dtm2 <- projectRaster(dtm, crs='+proj=longlat +datum=WGS84 +no_defs')
      chm2 <- projectRaster(chm, crs='+proj=longlat +datum=WGS84 +no_defs')

      library(data.table)
      fd1 <- dsf_list[names(dsf_list) ==names(x@header@VLR$Extra_Bytes$`Extra Bytes Description`)]

      chm23 <-  terra::resample(chm2,fd1[[1]][[1]], method = 'ngb')
      library(exactextractr)
      prec_chm <- exactextractr::exact_extract(chm23, sf, include_xy=T) %>%
        setNames(sf$treeID ) %>%
        invoke(rbind,.)%>%
        dplyr:: select(1:3) %>%
        as.data.frame()
      names(prec_chm)[1] <- 'chm'
      prec_chm <-  prec_chm %>%
        mutate(treeID =  sapply(strsplit( rownames(prec_chm),'[.]'), function(x){
          y=x[1]
        }))
      # tictoc::toc()
      # message(paste(names(fd1)))
      # tictoc::tic('spectra extraction')
      tesst <-  lapply(fd1, function(x11){
        x22 <-list(unlist(x11))
        lapply(x22, function(ls11){
          lapply(ls11, function(ls222){
            tryCatch({
              library(tictoc)
              tic("for loop start")
              message( paste0(names(ls222)))
              # print(ls222)
              # plot(ls222)
              # plot(sf,add=T, alpha=0.6,col=rainbow(1))
              library(exactextractr)
              prec_dfs <- exactextractr::exact_extract(ls222, sf, include_xy=T) %>%
                setNames(sf$treeID ) %>%
                invoke(rbind,.)%>%
                dplyr:: select(1) %>%
                as.data.frame()
              names(prec_dfs) <- names(ls222)
              print("finished")
              toc()
              return(prec_dfs)
            },
            error = function(e) {
              message('Caught an error!')
              cat("ERROR :", conditionMessage(e), "\n")
              print(e)}

            )})
        })
      }) %>%
        # map_dfr(cbind) %>%
        unlist(recursive = F)  %>%as.data.frame()
      tictoc::toc()
      names(tesst)
      dat_tes <- cbind(tesst,prec_chm)
      return(dat_tes)

    },error = function(e) {
      message('Caught an error!')
      cat("ERROR :", conditionMessage(e), "\n")
      print(e)},
    print(paste("processing Loop", names(las_list), sep = "_")))
  })
}





