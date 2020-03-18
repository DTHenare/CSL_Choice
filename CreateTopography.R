library(tidyverse)
library(akima)
library(scales)
library(mgcv)
library(gridExtra)
library(png)
library(grid)
library(patchwork)

dPaths = c("G:/My Drive/createToposData/","G:/My Drive/createToposData/Hanna/")

for (exp in 1:2) {
  
  #Set path to topoData
  dPath = dPaths[exp]
  #Create list of topoData files (one per person)
  fileList = list.files(dPath, pattern= "*_topoData.csv")
  winMin = -300
  winMax = 0
  scaleMin = -5
  scaleMax = 5
  #Choice keepers
  Keepers = c("2",  "5",  "6",  "7",  "8",  "9",  "10", "11" ,"12" ,"13", "14", "15", "16", "17", "18" ,"19" ,"20", "21", "22", "23", "24", "26")
  #Hanna keepers
  Keepers = c("1" , "2" , "3" , "4" , "6" , "7" , "8" , "9" , "10", "11" ,"12" ,"13", "14" ,"16" ,"17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28" ,"29", "30")
  
  for (subj in 1:length(fileList)) {
    #Load one topoData file and format
    topotest <- read_csv(paste(dPath,fileList[subj], sep = "")) %>%
      gather(electrode, amplitude, -Times)
    topotest$amplitude <- as.double(topotest$amplitude)
    topotest$Subject = subj
    
    if (subj == 1) {
      topoAll = topotest
    } else {
      topoAll = rbind(topoAll,topotest)
    }
    
  }
  
  topoAll = topoAll %>% group_by(Times, electrode) %>% summarise(Amplitude = mean(amplitude))
  rm(topotest,subj,fileList)
  gc()
  
  #Load the channel coordinate file and format
  electrodeLocs <- read_delim(paste(dPath,"CNV_Chanlocs.loc", sep = ""),
                              "\t",
                              escape_double = FALSE,
                              col_names = c("chanNo","theta","radius","electrode"),
                              trim_ws = TRUE)
  electrodeLocs$radianTheta <- pi/180*electrodeLocs$theta
  electrodeLocs <- electrodeLocs %>%
    mutate(x = .$radius*sin(.$radianTheta),
           y = .$radius*cos(.$radianTheta))
  #plot the channel locations
  ggplot(electrodeLocs,
         aes(x, y, label = electrode))+
    geom_text()+
    theme_bw()+
    coord_equal()
  #Craete theme to remove background lines from topoplot
  theme_topo <- function(base_size = 12)
  {
    theme_bw(base_size = base_size) %+replace%
      theme(
        rect             = element_blank(),
        line             = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_blank()
      )
  }
  #Add head outline to plot
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100) {
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  headShape <- circleFun(c(0, 0), round(max(electrodeLocs$x)), npoints = 100) # 0
  nose <- data.frame(x = c(-0.075,0,.075),y=c(.495,.575,.495))
  
  ggplot(headShape,aes(x,y))+
    geom_path()+
    geom_text(data = electrodeLocs,
              aes(x, y, label = electrode))+
    geom_line(data = nose,
              aes(x, y, z = NULL))+
    theme_topo()+
    coord_equal()
  
  #Bind locations to amplitudes for plotting
  allData <- topoAll %>% left_join(electrodeLocs, by = "electrode")
  
  #Define Matlab-style Jet colourmap
  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  
  #select a Timepoint
  singleTimepoint <- allData %>% filter(Times > winMin & Times <winMax, electrode != "vEOG") %>% group_by(electrode, chanNo, theta,radius,radianTheta,x,y) %>% summarise(Amplitude = mean(Amplitude))
  
  #Draw our map!
  ggplot(headShape,aes(x,y))+
    geom_path(size = 1.5)+
    geom_point(data = singleTimepoint,aes(x,y,colour = Amplitude),size = 3)+
    scale_colour_gradientn(colours = jet.colors(10),guide = "colourbar",oob = squish)+ #note: oob = squish forces everything outside the colour limits to equal nearest colour boundary (i.e. below min colours = min colour)
    geom_line(data = nose,aes(x, y, z = NULL),size = 1.5)+
    theme_topo()+
    coord_equal()
  
  ##### Interpolation MAtlab
  maskRing <- circleFun(diameter = 1.42)
  rmax <- .75   #specify a maximum boundary for the grid
  gridRes <- 67 #specify the interpolation grid resolution
  
  ## Create a function to perform Matlab's v4 interpolation.
  ## Takes as input a data-frame with columns x, y, and z (x co-ordinates, y co-ordinates, and amplitude)
  ## and variables xo and yo, the co-ordinates which will be use to create a grid for interpolation
  
  v4Interp <- function(df, xo, yo, rmax = .75, gridRes = 67) {
    xo <- matrix(rep(xo,length(yo)),nrow = length(xo),ncol = length(yo))
    yo <- t(matrix(rep(yo,length(xo)),nrow = length(yo),ncol = length(xo)))
    xy <- df$x + df$y*sqrt(as.complex(-1))
    d <- matrix(rep(xy,length(xy)),nrow = length(xy), ncol = length(xy))
    d <- abs(d - t(d))
    diag(d) <- 1
    g <- (d^2) * (log(d)-1)   # Green's function.
    diag(g) <- 0
    weights <- qr.solve(g,df$z)
    xy <- t(xy)
    outmat <- matrix(nrow = gridRes,ncol = gridRes)
    for (i in 1:gridRes){
      for (j in 1:gridRes) {
        test4 <- abs((xo[i,j] + sqrt(as.complex(-1))*yo[i,j]) - xy)
        g <- (test4^2) * (log(test4)-1)
        outmat[i,j] <- g %*% weights
      }
    }
    outDf <- data.frame(x = xo[,1],outmat)
    names(outDf)[1:length(yo[1,])+1] <- yo[1,]
    #return(list(x = xo[,1],y = yo[1,],z = outmat))
    return(outDf)
  }
  
  ## Create data frame to be used for interpolation - the function needs columns labelled x, y, and z
  
  testDat<- data.frame(x = singleTimepoint$x,
                       y = singleTimepoint$y,
                       z = singleTimepoint$Amplitude)
  
  #Create the interpolation grid
  xo <- seq(min(-rmax, testDat$x), max(rmax, testDat$x), length = gridRes)
  yo <- seq(max(rmax, testDat$y), min(-rmax, testDat$y), length = gridRes)
  
  interpV4 <- v4Interp(testDat, xo, yo)
  
  interpV4 <- gather(interpV4,
                     key = y,
                     value = Amplitude,
                     -x,
                     convert = TRUE) 
  
  interpV4$incircle <- (interpV4$x)^2 + (interpV4$y)^2 < 0.7 ^ 2 # mark
  
    if (exp == 1) {
    choiceTopo <- ggplot(interpV4[interpV4$incircle,],aes(x = x, y = y, fill = Amplitude))+
      geom_raster()+
      stat_contour(aes(z = Amplitude),binwidth = 0.5)+
      theme_topo()+
      geom_path(data = maskRing,
                aes(x, y, z = NULL, fill =NULL),
                colour = "white",
                size = 6)+
      scale_fill_gradientn(colours = jet.colors(10),
                           limits = c(scaleMin,scaleMax),
                           guide = "colourbar",
                           oob = squish)+
      geom_point(data = singleTimepoint,
                 aes(x,y),
                 size = 1)+
      geom_path(data = headShape,
                aes(x,y,z = NULL,fill = NULL),
                size = 1.5)+
      geom_path(data = nose,
                aes(x, y, z = NULL, fill = NULL),
                size = 1.5)+
      coord_equal() +
      xlab("Voluntary")
  } else {
    HannaTopo <- ggplot(interpV4[interpV4$incircle,],aes(x = x, y = y, fill = Amplitude))+
      geom_raster()+
      stat_contour(aes(z = Amplitude),binwidth = 0.5)+
      theme_topo()+
      geom_path(data = maskRing,
                aes(x, y, z = NULL, fill =NULL),
                colour = "white",
                size = 6)+
      scale_fill_gradientn(colours = jet.colors(10),
                           limits = c(scaleMin,scaleMax),
                           guide = "colourbar",
                           oob = squish)+
      geom_point(data = singleTimepoint,
                 aes(x,y),
                 size = 1)+
      geom_path(data = headShape,
                aes(x,y,z = NULL,fill = NULL),
                size = 1.5)+
      geom_path(data = nose,
                aes(x, y, z = NULL, fill = NULL),
                size = 1.5)+
      coord_equal()+
      xlab("Cued") +
      ggtitle("B") +
      theme(plot.title = element_text(family = "sans", size = 18, margin=margin(0,0,0,0)))
  }
}

load('CNV/plotData_readyforplotsandstats.RData')
fontSize = 12
x = c(-800,-500)
y=c(0,0)
lineData = data.frame(x=x,y=y)
levels(plotData$Chan) = c("Frontal", "Posterior")

ERPs <- plotData %>%
  group_by(sample, Chan, Exp) %>%
  summarise(mean = mean(mean)) %>%
  ggplot() +
  geom_rect(xmin = -300, xmax=-0, ymin = -Inf, ymax = Inf, size = 0, fill = "lemonchiffon") +
  geom_line(aes(sample, mean, colour = Exp),size=1) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~.,scales = "free_y") +
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_vline(xintercept = -500)+
  geom_hline(yintercept = 0,linetype = "dashed") +
  geom_line(data = lineData,aes(x=x,y=y)) +
  ggtitle("A") +
  theme_apa() +
  theme(panel.spacing.y = unit(3, "lines"), text= element_text(size=fontSize),
        axis.text.x = element_text(size = fontSize*0.93),
        axis.text.y = element_text(size = fontSize*0.93),
        legend.title = element_blank(),
        legend.text = element_text( size = fontSize*0.93),
        #legend.position = "bottom",
        plot.margin = unit(c(1,0,0,0),"cm"),
        plot.title = element_text(family = "sans", size = 18, margin=margin(0,0,0,0))
  )

final <- ERPs / (HannaTopo + choiceTopo+ plot_layout(guides = 'collect')) 
ggsave("Figures/comp.jpg", final, width = 16, height = 18, units = "cm", dpi = 300)

rm(topoAll,plotData, ERPs, HannaTopo, choiceTopo)
