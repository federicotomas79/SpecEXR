library(lidR)

ctg <- readLAScatalog("E:/PCGSPRO_1623035349/17366648332/新建可见光重建任务(11)//models/pc/0/terra_las/cloud.las",
                      select = "xyz"


)


x11()

las <-readLAS(ctg)

chm <- rasterize_canopy(las, res = 0.1, p2r())
plot(chm)



hf <- terra::draw(x="polygon", id=T)
df <- sf::st_as_sf(hf)
library(rgdal)
st_write(df, dsn="g:/exported_shaefile.shp", delete_layer = TRUE,layer="exported_shapefile", driver="ESRI Shapefile")





subset3 <- lidR::clip_roi(ctg,df)


plot(subset3)



opt_output_files(ctg) <- paste0(tempdir(),rnorm(1) ,"/{ORIGINALFILENAME}_{ID}")

opt_chunk_size(ctg) <- 0
opt_chunk_buffer(ctg) <- 20
classified_ctg <- classify_ground(ctg, csf())
dtm <- rasterize_terrain(classified_ctg, 1, tin())
plot (dtm)

ctg_norm <- normalize_height(classified_ctg, dtm)
plot (ctg_norm)


opt_select(ctg_norm) <- "xyz"
opt_filter(ctg_norm) <- "-keep_first"
hmean3 <- pixel_metrics(ctg_norm, ~mean(Z), 0.1)
hmean3[hmean3 < 0.1] <- NA



plot(hmean3, col = height.colors(25))
opt_output_files(ctg_norm) <-''
# ctg_norm@output_options$drivers$Raster$param$overwrite <- TRUE
# ttops <- locate_trees(ctg_norm, lmf(ws= kwsindice , hmin = hmin))
ttops <- locate_trees(hmean3,  lmf(ws= 4 , hmin = 2))



chm <- rasterize_canopy(ctg_norm, 0.1, p2r(0.15))
plot(chm)



# plot(chm, col = height.colors(50))
tictoc:: tic("processing segment_trees")
# ctg_norm$overwrite <- TRUE
opt_output_files(ctg_norm) <-paste0(tempdir(),rnorm(1),
                                    '{ORIGINALFILENAME}_{XCENTER}_{ID}' )
algo <- dalponte2016(chm, ttops)
ctg_segmented <- segment_trees(ctg_norm, algo)


ctg_segmented

# ctg_segmented$overwrite <- TRUE
opt_output_files(ctg_segmented) <- ''
crown_polo = crown_metrics(ctg_segmented, func = .stdtreemetrics, geom = "concave")
plot(sf::st_geometry(crown_polo), col = pastel.colors(250), axes = T)


df <- sf::st_as_sf( crown_polo$geometry)


tictoc:: tic("processing segment_trees")
# opt_output_files(ctg_segmented) <-paste0(tempdir(),
#                                          '{XCENTER}' )
subset3 <- lidR::clip_roi(ctg_segmented,df)
tictoc:: toc()





sf2  <-  crown_polo %>% dplyr:: filter(treeID ==72)
subset3 <- lidR::clip_roi(ctg_segmented,sf2)



pr <- terra::project(dsf1,paste0('EPSG:',epsg(lasukj)),res=0.1)
# tictoc:: toc()
crff  <- 1:length(subset3$filename)
crown_se  <- lapply(crff, function(fdx){
  expr <- tryCatch({

    # subset3 <- clip_roi(ctg_segmented, sf2$geometry)
    las3 = readLAS(subset3)

    fer <- payload(las3) %>% dplyr::select(X,Y,Z,treeID)
    fer$treeID <- as.factor(fer$treeID)
    names(fer) <- c('x','y','z','treeID')
    fer <- as.data.frame(fer)
    # sf2  <-  crowte %>% dplyr:: filter(treeID == fdx)
    sf2  <-  crowte %>% dplyr:: filter(treeID == unique(fer$treeID))

    # fsp <- sf_multipoint(fer)
    # fsp3 <- vect(fsp)
    message(paste0('project',fdx))
    # pr <- project(dsf1,'+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs',res=0.1
    dsta <- terra::extract(pr,fer[,c('x','y')],xy=T ) %>% mutate(treeID=fer$treeID,
                                                                 Z=fer$z,
                                                                 area=sf2$convhull_area
    ) %>%drop_na()
    return(dsta)

  },error = function(e){
    message('Caught an error!')
    paste(NaN)
    # print(e)
  })




















 lidR:::catalog_laxindex(ctg_segmented)
 tictoc:: tic("processing segment_trees")
 opt_output_files(ctg_segmented) <-paste0(tempdir(),
                                          '_{ID}' )
 subset4 <- lidR::clip_roi(ctg_segmented,df)
 tictoc:: toc()

 sf2  <-  crown_polo %>% dplyr:: filter(treeID ==98)
 # subset3 <- clip_roi(ctg_segmented, sf2$geometry)
 las3 = readLAS(subset3$filename)

 fer <- payload(las3) %>% dplyr::select(X,Y,Z,treeID)
 fer$treeID <- as.factor(fer$treeID)
 names(fer) <- c('x','y','z','treeID')
 fer <- as.data.frame(fer)%>%drop_na()

 sf2  <-  crown_polo %>% dplyr:: filter(treeID %in% fer$treeID)


 epsg(subset3)

 ctg5
 lasukj = readLAS(subset3$filename[1])


 ctg5 <- terra::rast("E:/PCGSPRO_1623035349/17366648332/新建可见光重建任务(11)/map/result.tif"


 )
 pr <- terra::project(ctg5,paste0('EPSG:',epsg(las3)),res=0.1)
 plot(pr,2)

 plot(fer[,c('x','y')],add=T)


 dsta <- terra::extract(pr,fer[,c('x','y')],xy=T ) %>% mutate(treeID=fer$treeID,
                                                              Z=fer$z
 ) %>%drop_na()

 dsta %>% filter(treeID ==15)  %>% ggplot(aes(x,Z,col=result_1))+geom_point()+
   scale_colour_viridis_c()



fer$treeID <- as.factor(fer$treeID)
names(fer) <- c('x','y','z','treeID')
fer <- as.data.frame(fer)



las = readLAS(LASfile, select = "xyz", filter = "-keep_first")
f <- system.file("extdata", "lake_polygons_UTM17.shp", package = "lidR")
lakes <- sf::st_read(f, quiet = TRUE)
subset3 <- clip_roi(las, lakes)








xy <- read.csv('E:/谷歌浏览器下载/e.csv')

x <- c(339348.8, 339141.9, 338579.6, 338520.8, 338110.0, 339385)
y <- c(5239254, 5238717, 5238169, 5239318, 5239247, 5239290)
r <- 3

plot(ctg_segmented)
points(x, y)
rois <- clip_circle(ctg_segmented, xy$x, xy$y, r)
plot(rois[[7]],axis=T,size=3)
names(rois) <- xy$z

chm <- rasterize_canopy(rois[[7]], 0.1, p2r(0.15))
plot(chm)

hmean3 <- pixel_metrics(rois[[7]], ~mean(Z), 0.1)
ttops <- locate_trees(hmean3,  lmf(ws= 4 , hmin = 2))

# ctg_norm$overwrite <- TRUE
opt_output_files(ctg_norm) <-paste0(tempdir(),rnorm(1),
                                    '{ORIGINALFILENAME}_{XCENTER}_{ID}' )
algo <- dalponte2016(chm, ttops)
ctg_segmented <- segment_trees(rois[[7]], algo)
plot(ctg_segmented)
crown_polo = crown_metrics(ctg_segmented, func = .stdtreemetrics, geom = "concave")
plot(sf::st_geometry(crown_polo), col = pastel.colors(250), axes = T)
library(sf)
directions_longlat <-  st_transform(crown_polo, '+proj=longlat +datum=WGS84 +no_defs')
#
sf  <- st_as_sf(directions_longlat)

sf  <- st_as_sf(crown_polo)


sf$convhull_area



sf2  <-  sf %>% dplyr:: filter(treeID == 143)

subset3 <- clip_roi(ctg_segmented, sf2$geometry)

# WGS 84 / UTM zone 50N

sp::plot(subset3, bg = "white", size = 5, axis = TRUE, legend = TRUE)

fer <- payload(subset3) %>% dplyr::select(X,Y,Z,treeID)
fer$treeID <- as.factor(fer$treeID)




library(lidR)

ctg4 <- readLAS("E:/PCGSPRO_1623035349/17366648332/新建可见光重建任务(10)//models/pc/0/terra_las/cloud.las"


)

plot(ctg4)
library(lidR)
sf_use_s2(FALSE)
# las2 <- st_transform(ctg4, st_crs('+proj=longlat +datum=WGS84 +no_defs'))
#


library(sfheaders)
library(terra)


ctg5 <- terra::rast("E:/PCGSPRO_1623035349/17366648332/新建多光谱重建任务(3)/map/result_Green.tif"


)

str(ctg5)
plot(pr)
fer
names(fer) <- c('x','y','z','treeID')
fer <- as.data.frame(fer)
fsp <- sf_multipoint(fer)

fsp3 <- vect(fsp)

plot(fsp3,add=T)


epsg(subset3)


pr <- terra::project(ctg5,paste0('EPSG:',epsg(subset3)),res=0.1)

dsta <- terra::extract(pr,fer[,c('x','y')],xy=T ) %>% mutate(treeID=fer$treeID,
                                            Z=fer$z)

ggplot(dsta,aes(y,Z,col=result_Green))+geom_point()+scale_color_viridis_c()










prec_chm <- exactextractr::exact_extract(pr, sf, include_xy=T) %>%
  setNames(paste0(sf$treeID,"_",sf$convhull_area,"_")) %>%
  invoke(rbind,.)%>%
  # %>% mutate(area=sf$convhull_area,
  #                            treeID= sf$treeID
  # )%>%
  # # dplyr:: select(1:3) %>%
  as.data.frame()



















library(tidyverse)
prec_chm <- exactextractr::exact_extract(chm, sf, include_xy=T  )  %>%
  setNames(paste0(sf$treeID,"_",sf$convhull_area,"_")) %>%
  # as.data.frame()%>%
  # prec_chm3 <-  append(prec_chm, sf$convhull_area)
 # mapply(append, ., sf$convhull_area,SIMPLIFY=FALSE )%>%
invoke(rbind,.)

  exactextractr::exact_extract(chm, sf, include_xy=F  )


rownames(prec_chm) <-  sapply(strsplit( rownames(prec_chm),'[_]'), function(x){
  y=paste0(x[1],x[3])
})


rownames(df)


length(prec_chm)


  sds <- prec_chm[[1]]
sum(sds$coverage_area)


  mutate(area=sf$convhull_area,
   treeID= sf$treeID
)%>%
  # setNames(sf$treeID) %>%

  invoke(rbind,.)%>% as_tibble()


%>% mutate(area=sf$convhull_area,
                             treeID= sf$treeID
                             )%>%

  # dplyr:: select(1:3) %>%
  as.data.frame()



prec_chm <-  prec_chm %>%
  mutate(treeID =  sapply(strsplit( rownames(prec_chm),'[_]'), function(x){
    y=x[1]
  }),
  carea =  sapply(strsplit( rownames(prec_chm),'[_]'), function(x){
    y=x[2]
  })

  )




List2 <- list(y1=rpois(99,10),y2=rpois(99,10))
new<- list(5,3)
List2<- mapply(append,List2,new,SIMPLIFY=FALSE)
List2
