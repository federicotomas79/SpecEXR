
#' SpecexR_app
#'this is an shiny app mainly for multispectral extraction using LAS point cloud data
#'
#'
#' @param ...
#'
#' @return final data with x,y location information
#' @export
#'
#' @examples  none
#'

SpecexR_app <- function(...) {
  list.of.packages <- c("librarian","Biobase","shinydashboard","tictoc",'BiocManager','quickPlot')
  new.packages <- list.of.packages[!(list.of.packages %in% utils::installed.packages()[,"Package"])]
  if(length(new.packages)) utils::install.packages(new.packages,repos = "https://cloud.r-project.org")
  if (!require("EBImage", quietly = TRUE))
    BiocManager::install('EBImage')
  library("EBImage")
  tictoc::tic()
  librarian::shelf("shinythemes",'RCSF','DT',"shinydashboard",'stars', 'sfheaders', 'sf','exactextractr', 'lidR', "shiny",'tidyverse','RStoolbox','viridis',
                   'raster','rdrop2','tools','rasterVis','data.table',quiet = T)
  tictoc::toc()

  print('data read time')
  raster_read  <- function(url) {
    lapply(url, function(urll){
      imag <- list.files(
        path = urll,
        pattern = '.tif',
        all.files = T,
        full.names = T,
        no.. = T
      )
      imag <- list(imag)
      # imag1 <-imag[-c(2,9)]
      lapply(imag, function(z)
        expr <- tryCatch({
          library(raster)
          tictoc::tic()
          p_dsm <- raster(z[grepl('dsm|Dsm', z)][1])
          p_blue <- raster::raster(z[grepl('Blue|blue', z)])
          p_green <- raster::raster(z[grepl('Green|green', z)])
          p_red <- raster::raster(z[grepl('Red|red', z)][1])
          p_redege <- raster::raster(z[grepl('dge|dge', z)])
          p_nir <-raster:: raster(z[grepl('NIR|nir', z)])
          p_dsm <- projectRaster(p_dsm, crs='+proj=longlat +datum=WGS84 +no_defs')
          p_dsm <- terra::resample(p_dsm,p_blue, method = 'ngb')
          trainx <- list(p_red,p_blue,p_green,p_redege,p_nir,p_dsm )
          names(trainx) <- c('red',"blue", "green",'redege','nir','dsm')
          tictoc::toc()
          return(trainx)
        },
        error = function(e) {
          message('Caught an error!')
          cat("ERROR :", conditionMessage(e), "\n")
          print(e)
        },
        print(paste("processing Loop", z, sep = "_"))))})}

  ###################
  # multi_rasl <- function(las_list,dsf_list,kwsindice,hmin){
  #   lapply(las_list, function(ctg){
  #     expr <- tryCatch({
  #       library("lidR")
  #       library("rgdal")
  #       library(sfheaders)
  #       library(stars)
  #       library(raster)
  #       library(tidyverse)
  #       library(sf)
  #       library(data.table)
  #       # message( paste0(ctg$gfdr))
  #       tictoc:: tic("processing las file")
  #       # opt_output_files(ctg) <- paste0(tempdir(), "{*}_ctgied")
  #       # opt_chunk_size(ctg) <- 0
  #       # opt_chunk_buffer(ctg) <- 40
  #       # classified_ctg <- classify_ground(ctg, csf())
  #       # dtm <- rasterize_terrain(classified_ctg, 1, tin())
  #       #
  #       opt_output_files(ctg) <- paste0(tempdir(),kwsindice,hmin, "_DhjvM_{*}")
  #       opt_chunk_size(ctg) <- 0
  #       opt_chunk_buffer(ctg) <- 40
  #       classified_ctg <- classify_ground(ctg, csf())
  #       dtm <- rasterize_terrain(classified_ctg, 1, tin())
  #       ctg_norm <- normalize_height(classified_ctg, dtm)
  #       opt_select(ctg_norm) <- "xyz"
  #       opt_filter(ctg_norm) <- "-keep_first"
  #       hmean3 <- pixel_metrics(ctg_norm, ~mean(Z), 0.5)
  #
  #
  #       plot(hmean3, col = height.colors(25))
  #
  #
  #       opt_output_files(ctg_norm) <-paste0(tempdir(),kwsindice,hmin, "_Dhgt_{*}")
  #       ttops <- locate_trees(ctg_norm, lmf(ws= kwsindice , hmin = hmin))
  #       chm <- rasterize_canopy(ctg_norm, 0.5, p2r(0.15))
  #
  #       plot(chm, col = height.colors(50))
  #
  #       opt_output_files(ctg_norm) <- paste0(tempdir(), kwsindice,hmin,"/D21ThjM_{*}")
  #       algo <- dalponte2016(chm, ttops)
  #       ctg_segmented <- segment_trees(ctg_norm, algo)
  #
  #
  #       opt_output_files(ctg_segmented) <- paste0(tempdir(), kwsindice,hmin,"/D2hTM_{*}")
  #       crown_polo = crown_metrics(ctg_segmented, func = .stdtreemetrics, geom = "convex")
  #       plot(sf::st_geometry(crown_polo), col = pastel.colors(250), axes = T)
  #
  #
  #       directions_longlat <-  st_transform(crown_polo, '+proj=longlat +datum=WGS84 +no_defs')
  #
  #       sf  <- st_as_sf(directions_longlat)
  #       sf33 <- as.data.frame(crown_polo)
  #       sf33 <- sf33%>%dplyr:: select(treeID,Z,convhull_area)%>% as.data.frame()
  #       dtm2 <- terra::project(dtm, '+proj=longlat +datum=WGS84 +no_defs')
  #       chm2 <- terra::project(chm, '+proj=longlat +datum=WGS84 +no_defs')
  #
  #       library(data.table)
  #       library(terra)
  #       # fd1 <- dsf_list
  #       # [names(dsf_list) ==ctg$gfdr]
  #       low <- rast(dsf_list[[1]]  )
  #       chm23 <-  terra::resample(chm2,low, method = 'near')
  #       # plot(chm23, col = height.colors(50))
  #       # plot(chm2, col = height.colors(50))
  #       library(exactextractr)
  #       prec_chm <- exactextractr::exact_extract(chm23, sf, include_xy=T) %>%
  #         setNames(sf$treeID ) %>%
  #         invoke(rbind,.)%>%
  #         dplyr:: select(1:3) %>%
  #         as.data.frame()
  #       names(prec_chm)[1] <- 'chm'
  #       prec_chm <-  prec_chm %>%
  #         mutate(treeID =  sapply(strsplit( rownames(prec_chm),'[.]'), function(x){
  #           y=x[1]
  #         }))
  #
  #       tesst <-  lapply(dsf_list, function(x11){
  #         x22 <-list(unlist(x11))
  #         lapply(x22, function(ls11){
  #           lapply(ls11, function(ls222){
  #             tryCatch({
  #               library(tictoc)
  #               tic("for loop start")
  #               message( paste0(names(ls222)))
  #               print(ls222)
  #               sp::plot(ls222)
  #               sp::plot(sf,add=T, alpha=0.6,col=rainbow(1))
  #               library(exactextractr)
  #               prec_dfs <- exactextractr::exact_extract(ls222, sf, include_xy=T) %>%
  #                 setNames(sf$treeID ) %>%
  #                 invoke(rbind,.)%>%
  #                 dplyr:: select(1) %>%
  #                 as.data.frame()
  #               names(prec_dfs)  <- names(ls222)
  #               print("finished")
  #               toc()
  #               return(prec_dfs)
  #             },
  #             error = function(e) {
  #               message('Caught an error!')
  #               cat("ERROR :", conditionMessage(e), "\n")
  #               print(e)}
  #
  #             )
  #           })
  #         })
  #       }) %>% unlist(recursive = F)  %>%as.data.frame()
  #       tictoc::toc()
  #       tesst <- tesst[rownames(prec_chm),]
  #       dat_tes <- cbind(tesst,prec_chm)
  #       return(dat_tes)
  #
  #     },error = function(e) {
  #       message('Caught an error!')
  #       cat("ERROR :", conditionMessage(e), "\n")
  #       print(e)},
  #     print(paste("processing Loop", names(las_list), sep = "_")))
  #   })
  # }



  data1 <- readr::read_rds(system.file("extdata", "data.rds", package = "SpectrEXR"))

  ras_im_alin <- function(monthi,fami){
    data1 <- readr::read_rds(system.file("extdata", "data.rds", package = "SpectrEXR"))

    expr <- tryCatch({
      library(tidyverse)
      library(raster)
      library(EBImage)
      library(tools)
      nir <- filter(data1, month == monthi )
      nir2 <- filter(nir, Fam == fami )
      nir3 <- nir2[,-c(1:4)]
      chl <- nir2[,4]
      library(tidyverse)
      matou_vis <- nir3 %>% dplyr:: mutate(
        ndvi=  ((result_NIR - result_Red) / (result_NIR + result_Red)),
        osavi = ((result_NIR-result_Red)*(1+0.16)) / (result_NIR + result_Red + 0.16),
        gndvi = (result_NIR-result_Green)/(result_NIR+result_Green),
        savi = ((result_NIR - result_Red)*(1+0.5))/((result_NIR + result_Red+0.5)),
        msavi = (2*result_NIR+1-sqrt((2*result_NIR+1)^2-8*(result_NIR-result_Red)))/2,
        gci = result_NIR/result_Green-1,
        RECI = result_NIR/result_RedEdge-1,
        LCI = (result_NIR-result_RedEdge)/(result_NIR+result_Red),
        GRVI =(result_Green-result_Red)/(result_Green+result_Red),
        MGRVI =(result_Green^2-result_Red^2)/(result_Green^2+result_Red^2 ),
        RGBVI =(result_Green^2-result_Red*result_Blue)/(result_Green^2+result_Red*result_Blue),
        NDRE= (result_NIR-result_RedEdge)/(result_NIR+result_RedEdge),
        MACI= result_NIR/result_Green,
        ARI= result_Green/result_NIR,
        MARI=(result_Green^(-1)-result_RedEdge^(-1))/result_NIR

      )

      tryCatch({
        library(raster)
        nir2 <- sapply(matou_vis[,-c(1:2,8:9)], function(x) (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm=T)))

        matou_vis2 <- cbind.data.frame(matou_vis[, c(1:2)], nir2)
        spg <- matou_vis2
        coordinates(spg) <- ~ x + y
        gridded(spg) <- TRUE
        rasterDF <- stack(spg)
        library(RStoolbox)
        library(rasterVis)
        library(viridis)
        df <-  ggRGB(rasterDF, r=1, g=5, b=3, stretch = 'lin')+ggtitle(paste0(monthi,"_", fami ))
        print(df)
        imgg <-raster:: as.array(rasterDF )
        ds3 <- EBImage::as.Image(imgg)
        ds3 <- ds3[,,-c(1:2)]
        ds4 <- resize(ds3,30,30)
        f_name  <- list(ds4)
        names(f_name) <- paste0(monthi,'_',fami)
        f_namechl  <- list(chl)
        names(f_namechl) <- paste0(monthi,'_chl_',fami)
        message( paste0(monthi,'_',fami))
        return(list(f_namechl,f_name))
      },error = function(e) {NULL})

    },error = function(e) {
      message('Caught an error!')
      cat("ERROR :", conditionMessage(e), "\n")
      print(e)})

  }



  month <- (unique(data1$month))
  fam <- (unique(data1$Fam))

  library(DT)

  ui <- navbarPage( h3("Forestry Phenomics"),position = c( "fixed-top" ),


                    tabPanel(h4("Segmentation"),
                             fluidPage(

                               theme = shinytheme("yeti"),
                               titlePanel("Individual tree segmentation"),
                               sidebarLayout(
                                 sidebarPanel(width = 4,
                                              selectInput("status", label = h3("fam data"),
                                                          choices = fam,
                                                          selected = fam[1], multiple = F),
                                              selectInput("status2", label =h3("Months data"),
                                                          choices = month,
                                                          selected = month[1], multiple = F),
                                              fluidRow(
                                                numericInput("width_png","Width of PNG", value = 1600) ,
                                                numericInput("height_png","Height of PNG", value = 1200 ),
                                                numericInput("resolution_PNG","Resolution of PNG", value = 144 ),
                                                style = "margin-top: 25px;",
                                                downloadButton('downloadPlotPNG','Download Layer plot PNG'),
                                                downloadButton('downloadPlotPNG2','Download singletree PNG')
                                              )


                                 ),

                                 mainPanel(

                                   plotOutput('distPlot',width = "100%", height = "800px"),
                                   tabsetPanel(type = "tabs",
                                               tabPanel("image info", verbatimTextOutput("summary")),
                                               tabPanel("Data", verbatimTextOutput("summary2")),
                                               tabPanel("Layer plot",plotOutput('predictPlot',width = "100%", height = "1000px"))


                                   )
                                 )
                               )
                             )
                    ),

                    tabPanel( h4("VIs graphs generation"),
                              fluidPage(

                                theme = shinytheme("yeti"),
                                titlePanel("Plot VIs"),
                                sidebarLayout(
                                  sidebarPanel(width = 4,

                                               column(
                                                 4,
                                                 fileInput(
                                                   "red1",
                                                   "Please upload an red images ",
                                                   multiple = T,
                                                   accept = c("tif",
                                                              ".tif")
                                                 )
                                               ),
                                               column(
                                                 4,
                                                 fileInput(
                                                   "gree1",
                                                   "Please upload a green images ",
                                                   multiple = T,
                                                   accept = c("tif",
                                                              ".tif")
                                                 )
                                               ),
                                               column(
                                                 4,
                                                 fileInput(
                                                   "blue1",
                                                   "Please upload a blue images ",
                                                   multiple = T,
                                                   accept = c("tif",
                                                              ".tif")
                                                 )
                                               ),
                                               column(
                                                 5,
                                                 fileInput(
                                                   "redege1",
                                                   "Please upload an redege images ",
                                                   multiple = T,
                                                   accept = c("tif",
                                                              ".tif")
                                                 )
                                               ),
                                               column(
                                                 5,
                                                 fileInput(
                                                   "NIR1",
                                                   "Please upload a NIR images ",
                                                   multiple = T,
                                                   accept = c("tif",
                                                              ".tif")
                                                 )
                                               ),

                                               column(12, wellPanel(style = "background: white",

                                                                    shinyWidgets::radioGroupButtons(
                                                                      "testh2o",
                                                                      h3("Plot Map:"),
                                                                      direction = "horizontal",
                                                                      individual = TRUE,
                                                                      width =
                                                                        '100%',
                                                                      justified =F,

                                                                      c(
                                                                        "Red" = "Red",
                                                                        "Green" = "Green",
                                                                        "Blue" = "Blue",
                                                                        "Rededage" = "Rededage",
                                                                        'NIR' = 'NIR',
                                                                        "ndvi" = "ndvi",
                                                                        "osavi" = "osavi",
                                                                        "gndvi" = "gndvi",
                                                                        "savi" = "savi",
                                                                        "msavi" = "msavi",
                                                                        "gci" = "gci",
                                                                        "RECI" = "RECI",
                                                                        "LCI" = "LCI",
                                                                        "GRVI" = "GRVI",
                                                                        "MGRVI" = "MGRVI",
                                                                        "NDRE" = "NDRE",
                                                                        "MACI" = "MACI",
                                                                        "ARI" = "ARI",
                                                                        "MARI" = "MARI"
                                                                      ),
                                                                      selected ="ndvi",
                                                                      checkIcon = list(yes = icon("ok",
                                                                                                  lib = "glyphicon"))
                                                                    )

                                               )),
                                               fluidRow(
                                                 column(4, numericInput("width_png2","Width of PNG", value = 1600)) ,
                                                 column(4, numericInput("height_png2","Height of PNG", value = 1200 )),
                                                 column(4, numericInput("resolution_PNG2","Resolution of PNG", value = 144 )),
                                                 column(4, numericInput("width_pdf","Width of pdf", value = 16)) ,
                                                 column(4, numericInput("height_pdf","Height of pdf", value = 12 )),

                                                 style = "margin-top: 25px;",
                                                 downloadButton('downloadPlotPNG11','Download single layer plot PNG'),
                                                 downloadButton('downloadPlotPNG22','Download RGB plot PDF')
                                               )


                                  ),

                                  # Show a plot of the generated distribution
                                  mainPanel( splitLayout(
                                    style = "border: 1px solid silver:",
                                    plotOutput("plotgraph1" , width = "100%", height = "800px"),
                                    plotOutput("plotgraph2", width = "100%", height = "800px")
                                  )

                                  )
                                )
                              )
                    ),

                    tabPanel(h4( "Tree identify and spectra extraction"),
                             fluidPage(

                               theme = shinytheme("yeti"),
                               # Application title
                               titlePanel("Segmentation"),
                               sidebarLayout(
                                 sidebarPanel(width = 4,
                                              column(6,fileInput("file1", "Choose las File",
                                                                 multiple = FALSE,
                                                                 accept = c("las",
                                                                            ".las"))),
                                              column(6,fileInput("file2", "Choose raster File",
                                                                 multiple = T,
                                                                 accept = c("tif",
                                                                            ".tif"))) ,
                                              fluidRow(
                                                column(4,downloadButton("tif_data", label = "Download spectral data")),
                                                column(4, downloadButton("cleaned_data", label = "Download las data"))) ,
                                              # Horizontal line ----
                                              tags$hr(),

                                              fluidRow(
                                                column(4, numericInput("width_png3","Width of PNG", value = 1600)) ,
                                                column(4, numericInput("height_png3","Height of PNG", value = 1200 )),
                                                column(4, numericInput("resolution_PNG3","Resolution of PNG", value = 144 )),
                                                column(4, numericInput("width_pdf3","Width of pdf", value = 16)) ,
                                                column(4, numericInput("height_pdf3","Height of pdf", value = 12 )),

                                                style = "margin-top: 25px;"

                                              )
                                 ) ,

                                 mainPanel(
                                   h1("Introducing Shiny"),
                                   p("Please use cloud point",
                                     em("(.las)"),
                                     "and raster images",
                                     em("(.tif)"),
                                     "to start the extraction"),
                                   fluidRow( column(3,   sliderInput(inputId = 'wscontro',
                                                                     label = 'find tree ws control:',
                                                                     value = 6,
                                                                     step =0.1,
                                                                     min =0 ,
                                                                     max =10)),
                                             column(3,   sliderInput(inputId = 'hmincor',
                                                                     label = 'find tree hmin control:',
                                                                     value = 2,
                                                                     step =0.1,
                                                                     min =0 ,
                                                                     max =10)),
                                             column(3, downloadButton('downloadrgball',
                                                                      'Download',class = "butt1"),
                                             ) ,
                                             column(3,    sliderInput("select2",
                                                                      "TreeID number:",
                                                                      min = 1,
                                                                      max = 2000,
                                                                      step =1,
                                                                      value = 20)),
                                             column(3, downloadButton('downloadsfall','Download') )
                                   ) ,
                                   fluidRow( column(12, splitLayout(
                                     style = "border: 1px solid silver:",
                                     plotOutput("predictPlo" , width = "100%", height = "800px"),
                                     plotOutput("predictPlot5", width = "100%", height = "800px")
                                   ))),
                                   tableOutput("contents2"),
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Point cloud information", verbatimTextOutput("summar")),
                                               tabPanel("Spectral information", verbatimTextOutput("summar2")),
                                               tabPanel("LAS polt information",
                                                        sidebarPanel(
                                                          fluidRow(
                                                            actionButton("dodo", "plot las point cloud"),
                                                            actionButton("dodo1", "plot single tree point cloud")
                                                          )),
                                                        mainPanel(tableOutput("contents"),
                                                                  tableOutput("contents22"))),


                                               # tabPanel("Magic",plotOutput('distPlo',width = "100%", height = "1000px")),
                                               tabPanel("Layer&RGB plot",
                                                        wellPanel(fluidRow(
                                                          column(12,sliderInput(inputId = 'heightdata',
                                                                                label = 'height limits:',
                                                                                value = 0,
                                                                                step =0.1,
                                                                                min =0,
                                                                                max =10))
                                                        )),
                                                        fluidRow( column(12, splitLayout(
                                                          style = "border: 1px solid silver:",
                                                          plotOutput("predictPlot3" , width = "100%", height = "800px"),
                                                          plotOutput("predictPlot4", width = "100%", height = "800px")
                                                        ))),

                                               ),
                                               # tabPanel("Single tree RGB plot",plotOutput('predictPlot4',width = "100%", height = "1000px")),

                                               tabPanel("Final data output",

                                                        sidebarPanel(
                                                          fluidRow(
                                                            column(4, downloadButton("tife_data", label = "Downloadc final data"))
                                                          )),
                                                        mainPanel(verbatimTextOutput("su2")))

                                   )
                                 )
                               )
                             )

                    )





  )




  library(tools)
  options(shiny.maxRequestSize=1000*1024^2)
  server <- function(input, output) {
    trend_data <- reactive({
      sele <-input$status
    })
    trend_data2 <- reactive({
      selec <-input$status2
    })

    output$summary <- renderPrint({
      chl_tree <- sigletree()
      gfdgh1 <- chl_tree %>%unlist(recursive = F)
      gfdgh1[sapply(gfdgh1, is.null)] <- NULL
      library(data.table)
      fffghj2 <- (gfdgh1[!names(gfdgh1) %like% 'chl'])
      fffghj2[sapply(fffghj2, is.null)] <- NULL
      fffghj2[[1]][is.na(fffghj2[[1]])] <- 0
      print(fffghj2[[1]])

    })


    output$distPlot <- renderPlot({
      chl_tree <- sigletree()
      print(chl_tree)

    })


    sigletree <- reactive({

      chl_treew <-  ras_im_alin(fami= trend_data(),monthi=trend_data2())
      chl_treew

    })


    output$summary2  <- renderPrint({
      chl_tree <- sigletree()
      gfdgh1 <- chl_tree %>%unlist(recursive = F)
      gfdgh1[sapply(gfdgh1, is.null)] <- NULL
      library(data.table)
      stOdds <- (gfdgh1[names(gfdgh1)%like% 'chl'])%>% invoke(rbind,.)
      spl <- strsplit(rownames(stOdds),'_|[.]')
      stOdds$name <- sapply(spl, function(x){
        y=(paste0(x[1],'_',x[3],'_',x[4],'_',x[5]))
      })
      stOdds

    })

    output$predictPlot <- renderPlot({
      chl_tree <- sigletree()
      gfdgh1 <- chl_tree %>%unlist(recursive = F)
      gfdgh1[sapply(gfdgh1, is.null)] <- NULL
      library(data.table)
      fffghj2 <- (gfdgh1[!names(gfdgh1) %like% 'chl'])
      fffghj2[sapply(fffghj2, is.null)] <- NULL
      fffghj2[[1]][is.na(fffghj2[[1]])] <- 0
      y <- brick(fffghj2[[1]])
      library(viridis)
      sp::plot(y,col=viridis(20))


    })

    df_products_upload <- reactive({
      inFile <- input$file1
      if (is.null(inFile))
        return('please upload las cloud data')
      las_12 <- lapply(inFile$datapath,function(m){
        fdd <- lidR::readLAScatalog(m )
        # fdd$gfdr <- 'month'

      } )

      las_12

    })



    testedd   <- reactive({
      chl_tree <- ras_im_alin(fami= trend_data(),monthi=trend_data2())

      gfdgh1 <- chl_tree %>%unlist(recursive = F)%>%unlist(recursive = F)%>%unlist(recursive = F)
      gfdgh1[sapply(gfdgh1, is.null)] <- NULL
      fffghj<-gfdgh1%>%unlist(recursive = F)%>%unlist(recursive = F)
      library(data.table)

      fffghj2 <- (fffghj[!names(fffghj) %like% 'chl'])
      fffghj2[sapply(fffghj2, is.null)] <- NULL
      fffghj2[[1]][is.na(fffghj2[[1]])] <- 0
      y <- brick(fffghj2[[1]])
      library(viridis)
      sp::plot(y,col=viridis(20))


    })


    output$downloadPlotPNG <- downloadHandler(
      filename = function() {
        x <- gsub(":", ".", Sys.time())
        paste("ggVolcanoR_", gsub("/", "-", x), ".png", sep = "")
      },
      content = function(file) {

        png(file, width = input$width_png, height = input$height_png, res = input$resolution_PNG)
        testedd()
        dev.off()
      },

      contentType = "application/png" # MIME type of the image

    )


    output$downloadPlotPNG2 <- downloadHandler(
      filename = function() {
        x <- gsub(":", ".", Sys.time())
        paste("ggVolcanoR_", gsub("/", "-", x), ".png", sep = "")
      },
      content = function(file) {

        png(file, width = input$width_png, height = input$height_png, res = input$resolution_PNG)
        ras_im_alin(fami= trend_data(),monthi=trend_data2())
        dev.off()
      },

      contentType = "application/png"

    )


    getData <- reactive({
      library(data.table)
      inFile1 <- input$file2
      if (is.null(inFile1))
        return(print('please upload raster images'))
      plr <- lapply(inFile1$datapath, function(m){
        rs <- raster(m )
        rs
      })

      for (i in 1:length(plr)) {
        names(plr[[i]]) <- inFile1$name[[i]]
      }

      plr

    })


    output$cleaned_data <- downloadHandler(
      filename = "mydata.rds",
      content = function(file) {readr::write_rds(df_products_upload(), file)}
    )


    # data_las <- reactive({
    #   select <-input$caption2
    #   imag <- list.files(
    #     path = select,
    #     pattern = '*.las',
    #     all.files = T,
    #     full.names = T,
    #     no.. = T
    #   )
    #   library("lidR")
    #   library("rgdal")
    #   library(raster)
    #   library(tidyverse)
    #   las_12 <-lidR:: readLAScatalog(imag[1],
    #                           filter = "-change_classification_from_to 1 2",
    #                           select = "xyzirc" )
    # })

    output$summar  <- renderPrint({
      las22 <- df_products_upload()
      print(las22)
    })



    data_dsf1 <- reactive({
      select1 <-input$caption
      library("lidR")
      library("rgdal")
      library(raster)
      library(tidyverse)
      dsf1 <- raster_read(select1)
      dsf1 <- dsf1 %>% unlist(recursive = F) %>%  unlist(recursive = F)

    })


    output$summar2  <- renderPrint({
      dsf1 <-  getData()
      if (is.null(dsf1))
        return(NULL)
      print(dsf1)
    })

    # output$distPlo <- renderPlot({
    #   select23 <-  getData()
    #   if (is.null(select23))
    #     return(NULL)
    #   sp::plot( stack(select23),col= viridis(200))
    #
    # })


    data_ext2  <- reactive({
      dsf1 <- getData()
      las_12 <- df_products_upload()
      if (is.null(dsf1))
        return('please upload raster images')
      if (is.null(las_12))
        return('please upload las cloud data')
      las_list <- las_12
      # names(las_list) <- c('month')
      dsf_list <- list(dsf1)
      # names(dsf_list) <- c('month')
      par(mfrow = c(2,3))
      data_all <- multi_rasl (las_list,dsf_list,kwsindice = input$wscontro, hmin = input$hmincor )

    })


    data_ext2  <- reactive({
      dsf1 <- getData()
      las_12 <- df_products_upload()
      if (is.null(dsf1))
        return('please upload raster images')
      if (is.null(las_12))
        return('please upload las cloud data')
      las_list <- las_12
      # names(las_list) <- c('month')
      dsf_list <- list(dsf1)
      # names(dsf_list) <- c('month')
      par(mfrow = c(2,3))
      data_all <- mult()[[3]]

    })

    mult  <-  reactive({
      las_12 <- df_products_upload()
      las_list <- las_12
      # names(las_list) <- c('month')
      dsf1 <- getData()
      dsf_list <- list(dsf1)
      par(mfrow = c(2,3))
      lasdata <-   lapply(las_list, function(ctg){
        expr <- tryCatch({
          library("lidR")
          library("rgdal")
          library(sfheaders)
          library(stars)
          library(raster)
          library(tidyverse)
          library(sf)
          library(data.table)
          # message( paste0(ctg$gfdr))
          tictoc:: tic("processing las file")

          # opt_output_files(ctg) <- paste0(tempdir(), "{*}_ctfied")
          # opt_chunk_size(ctg) <- 0
          # opt_chunk_buffer(ctg) <- 40
          # classified_ctg <- classify_ground(ctg, csf())
          # dtm <- rasterize_terrain(classified_ctg, 1, tin())

          opt_output_files(ctg) <-paste0(tempdir(), input$wscontro,input$hmincor ,"/fdfytf_{*}")
          opt_chunk_size(ctg) <- 0
          opt_chunk_buffer(ctg) <- 40
          classified_ctg <- classify_ground(ctg, csf())
          dtm <- rasterize_terrain(classified_ctg, 1, tin())
          ctg_norm <- normalize_height(classified_ctg, dtm)
          opt_select(ctg_norm) <- "xyz"
          opt_filter(ctg_norm) <- "-keep_first"
          # hmean3 <- pixel_metrics(ctg_norm, ~mean(Z), 0.5)


          # plot(hmean3, col = height.colors(25))


          opt_output_files(ctg_norm) <-paste0(tempdir(), input$wscontro,input$hmincor,"/jytyhfhTM_{*}")
          # ttops <- locate_trees(ctg_norm, lmf(ws= kwsindice , hmin = hmin))
          ttops <- locate_trees(ctg_norm,  lmf(ws= input$wscontro , hmin = input$hmincor))
          chm <- rasterize_canopy(ctg_norm, 0.5, p2r(0.15))

          # plot(chm, col = height.colors(50))

          opt_output_files(ctg_norm) <- paste0(tempdir(), input$wscontro,input$hmincor ,"/DT4tyrMdf_{*}")
          algo <- dalponte2016(chm, ttops)
          ctg_segmented <- segment_trees(ctg_norm, algo)
          #

          opt_output_files(ctg_segmented) <- paste0(tempdir(), input$wscontro,input$hmincor,"/D2hytTM_{*}")
          crown_polo = crown_metrics(ctg_segmented, func = .stdtreemetrics, geom = "convex")
          plot(sf::st_geometry(crown_polo), col = pastel.colors(250), axes = T)


          directions_longlat <-  st_transform(crown_polo, '+proj=longlat +datum=WGS84 +no_defs')

          sf  <- st_as_sf(directions_longlat)
          sf33 <- as.data.frame(crown_polo)
          sf33 <- sf33%>%dplyr:: select(treeID,Z,convhull_area)%>% as.data.frame()
          dtm2 <- terra::project(dtm, '+proj=longlat +datum=WGS84 +no_defs')
          chm2 <- terra::project(chm, '+proj=longlat +datum=WGS84 +no_defs')

          library(data.table)
          library(terra)
          # fd1 <- dsf_list
          # [names(dsf_list) ==ctg$gfdr]
          low <- rast(dsf_list[[1]]  )
          chm23 <-  terra::resample(chm2,low, method = 'near')
          # plot(chm23, col = height.colors(50))
          # plot(chm2, col = height.colors(50))
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

          tesst <-  lapply(dsf_list, function(x11){
            x22 <-list(unlist(x11))
            lapply(x22, function(ls11){
              lapply(ls11, function(ls222){
                tryCatch({
                  library(tictoc)
                  tic("for loop start")
                  message( paste0(names(ls222)))
                  print(ls222)
                  sp::plot(ls222)
                  sp::plot(sf,add=T, alpha=0.6,col=rainbow(1))
                  library(exactextractr)
                  prec_dfs <- exactextractr::exact_extract(ls222, sf, include_xy=T) %>%
                    setNames(sf$treeID ) %>%
                    invoke(rbind,.)%>%
                    dplyr:: select(1) %>%
                    as.data.frame()
                  names(prec_dfs)  <- names(ls222)
                  print("finished")
                  toc()
                  return(prec_dfs)
                },
                error = function(e) {
                  message('Caught an error!')
                  cat("ERROR :", conditionMessage(e), "\n")
                  print(e)}

                )
              })
            })
          }) %>% unlist(recursive = F)  %>%as.data.frame()
          tictoc::toc()
          tesst <- tesst[rownames(prec_chm),]
          dat_tes <- cbind(tesst,prec_chm)
          return(list(crown_polo,sf,dat_tes))


        },error = function(e) {
          message('Caught an error!')
          cat("ERROR :", conditionMessage(e), "\n")
          print(e)},
        print(paste("processing Loop", names(las_list), sep = "_")))
      })%>% unlist(recursive = F)

      lasdata
    })

    # crown_pol  <-  reactive({
    #   crown_polo  <- mult()
    #   crown_polo1  <- lapply(crown_polo, function(x){
    #     opt_output_files(x) <- paste0(tempdir(), input$wscontro,input$hmincor ,"/G5kjh5f_{*}")
    #     crown_p <-  crown_metrics(x, func = .stdtreemetrics, geom = "convex")
    #     crown_p
    #   })
    #
    #
    # })


    sflas  <-  reactive({
      crowtemp  <- mult()[[2]]
      # crown_pow  <- lapply(crowtemp, function(x){
      #   opt_output_files(x) <- paste0(tempdir(), input$wscontro,input$hmincor ,"/fdfM_{*}")
      #   crowtemp1  <- crown_metrics(x, func = .stdtreemetrics)
      #   directions_longlat <- st_transform(crowtemp1, '+proj=longlat +datum=WGS84 +no_defs')
      #   # spTransform(crowtemp1,sp:: CRS("+proj=longlat +datum=WGS84 +no_defs"))
      #   sf  <- st_as_sf(directions_longlat)
      # })
      #
      # crown_pow


    })


    sertree  <-  reactive({
      crowte  <- mult()
      crown_se  <- lapply(crowte, function(x){
        idnum <- x[x$treeID ==  input$select2,]
        library(tidyverse)
        p1<- idnum %>% filter( Z >2)%>%
          ggplot() + geom_sf( )
        # p3<- p1 +
        #   geom_sf(data = idnum,col='red')+
        #   # geom_sf_text(data=xx, aes(label = treeID),size=2)+
        #
        #   # geom_sCf_text(data=idnum, aes(label = treeID),col='red',size=2)
        p1




      })

      crown_se


    })



    randse <- eventReactive(input$dodo1, {
      sele <- sertree()

    })


    output$contents  <- renderPrint({
      library("lidR")
      library("rgdal")
      library(raster)
      library(tidyverse)
      print("plot with RGL device")
      randese <-  randse()
      if (is.null(randese))
        return(NULL)

      randese1  <- lapply(randese, function(xyr){
        sp:: plot(xyr, size = 8, bg = "white",breaks='quantile' ,
                  axis = TRUE )
      })
    })


    plot2  <- reactive({
      fdff <- mult()[[1]]
      if (is.null(fdff))
        return(NULL)
      dre <- lapply(fdff, function(xx){
        idnum <- xx[xx$treeID ==  input$select2,]

        p1<-  ggplot() + geom_sf(data = xx)+
          geom_sf(data = idnum,col='red')+
          geom_sf_text(data=xx, aes(label = treeID),size=2)+

          geom_sf_text(data=idnum, aes(label = treeID),col='red',size=2)
        p1

      })

      dre
    })


    plot23  <- reactive({
      fdff <- mult()
      if (is.null(fdff))
        return(NULL)
      dre <- lapply(fdff, function(xx){
        idnum <- xx[xx$treeID ==  input$select2,]
        p1<-  ggplot() + geom_sf(data = xx)+
          geom_sf(data = idnum,col='red')+
          # geom_sf_text(data=xx, aes(label = treeID),size=2)+

          geom_sf_text(data=idnum, aes(label = treeID),col='red',size=2)
        p1


      })
      dre

    })


    output$predictPlot5  <- renderPlot({
      fdff2 <-  plot23()
      print(fdff2)
      # fdff2
    })

    output$downloadsfall  <- downloadHandler(
      filename = function() {
        x <- gsub(":", ".", Sys.time())
        paste("spetral_", gsub("/", "-", x), ".png", sep = "")
      },
      content = function(file) {

        png(file, width = input$width_png3, height = input$height_png3, res = input$resolution_PNG3)
        print(plot2())
        dev.off()
      },

      contentType = "application/png"

    )



    rgbplotwithid  <- reactive({

      sfff <- sflas()
      select23 <-  getData()
      if (is.null(sfff))
        return(NULL)
      if (is.null(select23))
        return(NULL)
      se2 <-  stack(select23)

      if(nlayers(se2) < 3){
        sp:: plot(se2[[1]] ,col= viridis(200) )

        lapply(sfff,function(xyxy){
          idnum <- xyxy[xyxy$treeID ==  input$select2,]
          # p1<-  ggplot() + geom_sf(data = xyxy)+
          #   geom_sf(data = idnum,col='red')+
          #   # geom_sf_text(data=xx, aes(label = treeID),size=2)+
          #
          #   geom_sf_text(data=idnum, aes(label = treeID),col='red',size=2)
          # p1

          sp::plot(xyxy,add=T,col='white',alpha=0.4)

          idnum <- xyxy[xyxy$treeID == input$select2,]
          sp::plot(idnum, add=T,alpha=0.4,col='red')
          text(idnum, paste(idnum$treeID ),
               cex=1,col='blue',alpha=0.4)

        })
      } else{

        lapply(sfff,function(xyxy){
          library(RStoolbox)
          idnum <- xyxy[xyxy$treeID == input$select2,]
          p <-ggRGB(se2,  stretch = "hist")+
            geom_sf(data = xyxy, fill='orange',alpha=0.4   )+
            geom_sf(data = idnum, fill='red')+
            ggrepel::geom_label_repel(
              data = idnum,
              aes(label = treeID, geometry = geometry),
              stat = "sf_coordinates",
              min.segment.length = 0,
              colour = "orange",
              segment.colour = "orange"
            )


          print(p)

        })

      }

    })



    output$downloadrgball  <- downloadHandler(


      filename = function() {
        x <- gsub(":", ".", Sys.time())
        paste("spetral_", gsub("/", "-", x), ".png", sep = "")
      },
      content = function(file) {
        rgbg <- rgbplotwithid()
        png(file, width = input$width_png3, height = input$height_png3, res = input$resolution_PNG3)
        print(rgbg)
        dev.off()
      },

      contentType = "application/png"

    )



    output$predictPlo  <- renderPlot({
      rgbplotwithidw<- rgbplotwithid()
      print(rgbplotwithidw)

    })

    output$tife_data <- downloadHandler(
      filename = "finaloutput data.rds",
      content = function(file) {readr::write_rds(data_ext2(), file)}
    )


    finaldata  <- reactive({
      dsf1 <-  data_ext2()
      if (is.null(dsf1))
        return(NULL)
      data_Jan <- dsf1 %>% invoke(cbind,.)
      names(data_Jan) <-  gsub('month|[.]|tif','',names(data_Jan))
      data_Jan <- data_Jan %>% dplyr::select(x,y,treeID,chm, everything())
      data_Jan
    })


    output$su2  <- renderPrint({
      dsf331 <-  mult()
      print(dsf331)
    })




    output$predictPlot3  <- renderPlot({
      library(tidyverse)
      library(raster)
      library(EBImage)
      library(tools)
      nir <- filter(finaldata(), treeID == input$select2 )
      nir3 <- nir[,-c(1:4)]
      library(raster)
      nir2 <- sapply(nir3, function(x) (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm=T)))

      matou_vis2 <- cbind.data.frame(nir[,c(1:2,4)], nir2)
      matou_vis2 <- matou_vis2 %>% filter(chm > input$heightdata)
      spg <- matou_vis2
      coordinates(spg) <- ~ x + y
      gridded(spg) <- TRUE
      rasterDF <- stack(spg)
      library(RStoolbox)
      library(rasterVis)
      library(viridis)
      sp:: plot(rasterDF ,col= viridis(200))

    })


    output$predictPlot4  <- renderPlot({
      library(tidyverse)
      library(raster)
      library(EBImage)
      library(tools)
      nir <- filter(finaldata(), treeID == input$select2 )
      nir3 <- nir[,-c(1:4)]
      library(raster)
      nir2 <- sapply(nir3, function(x) (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm=T)))

      matou_vis2 <- cbind.data.frame(nir[,c(1:2,4)], nir2)
      matou_vis2 <- matou_vis2 %>% filter(chm > input$heightdata)
      spg <- matou_vis2
      coordinates(spg) <- ~ x + y
      gridded(spg) <- TRUE
      rasterDF <- stack(spg)
      library(RStoolbox)
      library(rasterVis)
      library(viridis)

      if (nlayers(rasterDF) < 3) {
        print("at least 3 layers needed for RGB plot")


      } else{

        if (nlayers(rasterDF)>3|nlayers(rasterDF)< 5) {
          df <-  ggRGB(rasterDF,  2, 3, 4,
                       stretch = 'lin') + ggtitle(paste0('tree ID-', input$select2))
          print(df)

        }else{

          df <-  ggRGB(rasterDF, 5, 3, 2,
                       stretch = 'lin') + ggtitle(paste0('tree ID-', input$select2))
          print(df)


        }
      }
    })



    randomVals <- eventReactive(input$dodo, {
      sele <-df_products_upload()

    })


    output$contents22 <- renderPrint({
      library("lidR")
      library("rgdal")
      library(raster)
      library(tidyverse)
      print("plot with RGL device")
      lapply(randomVals(),  sp::plot )
    })


    red2 <-  reactive({
      library(raster)
      sele1 <- input$red1
      dsf1 <- raster::raster(sele1$datapath)
    })
    green2 <-  reactive({
      library(raster)
      sele2 <- input$gree1
      dsf2 <- raster::raster(sele2$datapath)
    })
    blue2 <-  reactive({
      library(raster)
      sele3 <- input$blue1
      dsf3 <- raster::raster(sele3$datapath)
    })
    redege2 <-  reactive({
      library(raster)
      sele4 <- input$redege1
      dsf4 <- raster::raster(sele4$datapath)
    })
    NIR2 <-  reactive({
      library(raster)
      sele5 <- input$NIR1
      dsf5 <- raster::raster(sele5$datapath)
    })

    ndviind <- reactive({
      st1 <-  ((NIR2() - red2()) / (NIR2() + red2()))
    })
    osavi <- reactive({
      osavi = ((NIR2() - red2()) * (1 + 0.16)) / (NIR2() + red2() + 0.16)
    })
    gndvi <- reactive({
      gndvi = (NIR2() - green2()) / (NIR2() + green2())
    })
    savi <- reactive({
      savi = ((NIR2() - red2()) * (1 + 0.5)) / ((NIR2() + red2() + 0.5))
    })
    msavi <- reactive({
      msavi = (2 * NIR2() + 1 - sqrt((2 * NIR2() + 1) ^ 2 - 8 * (NIR2() - red2()))) /
        2
    })
    gci <- reactive({
      gci = NIR2() / green2() - 1
    })
    RECI <- reactive({
      RECI = NIR2() / redege2() - 1
    })
    LCI <- reactive({
      LCI = (NIR2() - redege2()) / (NIR2() + red2())
    })
    GRVI <- reactive({
      GRVI = (green2() - red2()) / (green2() + red2())
    })
    MGRVI <- reactive({
      MGRVI = (green2() ^ 2 - red2() ^ 2) / (green2() ^ 2 + red2() ^ 2)
    })
    RGBVI <- reactive({
      RGBVI = (green2() ^ 2 - red2() * blue2()) / (green2() ^ 2 + red2() * blue2())
    })

    NDRE <- reactive({
      NDRE = (NIR2() - redege2()) / (NIR2() + redege2())
    })
    MACI <- reactive({
      MACI = NIR2() / green2()
    })

    ARI <- reactive({
      ARI = green2() / NIR2()
    })

    MARI <- reactive({
      MARI = (green2() ^ (-1) - redege2() ^ (-1)) / NIR2()
    })

    dat243 <- reactive({
      select <- switch(
        input$testh2o,
        "Red" = red2(),
        "Green" = green2(),
        "Blue" = redege2(),
        "Rededage" = blue2(),
        "NIR" = NIR2() ,
        "ndvi" = ndviind(),
        "osavi" = osavi(),
        "gndvi" = gndvi(),
        "savi" = savi(),
        "msavi" = msavi(),
        "gci" = gci(),
        "RECI" = RECI(),
        "LCI" = LCI(),
        "GRVI" = GRVI(),
        "MGRVI" = MGRVI(),
        "NDRE" = NDRE(),
        "MACI" = MACI(),
        "ARI" = ARI(),
        "MARI" = MARI()
      )
    })

    pyt <- reactive({
      st1 <- raster::brick(  blue2(), green2(),red2(), redege2(), NIR2())

    })
    output$plotgraph1 <- renderPlot({

      library(RStoolbox)
      library(raster)
      pyt2 <-  RStoolbox::ggRGB(pyt(),

                                stretch  = 'hist')
      print(pyt2)

    })

    output$plotgraph2 <- renderPlot({
      library(RStoolbox)
      library(raster)

      sp::plot(dat243())

    })

    output$downloadPlotPNG11  <- downloadHandler(
      filename = function() {
        x <- gsub(":", ".", Sys.time())
        paste("spetral_", gsub("/", "-", x), ".png", sep = "")
      },
      content = function(file) {

        png(file, width = input$width_png2, height = input$height_png2, res = input$resolution_PNG2)
        sp::plot(dat243())
        dev.off()
      },

      contentType = "application/png"

    )

    output$downloadPlotPNG22  <- downloadHandler(
      filename = function() {
        x <- gsub(":", ".", Sys.time())
        paste("spetral_", gsub("/", "-", x), ".pdf", sep = "")
      },

      content = function(file) {

        pdf(file, width  = input$width_pdf, height  = input$height_pdf )
        pf<-  ggRGB(pyt(), stretch  = 'hist')
        print(pf)
        dev.off()
      },

      contentType = "application/pdf"

    )


  }

  app <- shinyApp(ui = ui, server = server)
  runApp(app, launch.browser = TRUE)

}

SpecexR_app()

