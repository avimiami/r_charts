#library(magick)
library(ggplot2)
library(ggthemes)
library(quantmod)


getXTS_tiingo= function(sym_in, adj=TRUE){
  sym_in = gsub(" ", "", sym_in)
  x1 = getSymbols(sym_in, src ='tiingo', auto.assign = FALSE, api.key='insert-yours-here', adjust = adj, from='2007-01-01',
                  to=Sys.Date())
  colnames(x1) = c( "Open", "High", "Low", "Close", "Volume")

  return(x1)
}

sym_in = 'nio'
sym_in_Text = 'Nio'
mainDIR = getwd()

saveDIR = paste0(mainDIR, "/", sym_in,"_" , Sys.Date())


if (file.exists(saveDIR) ==FALSE){
  dir.create(saveDIR)
} 

out_folder_images =  saveDIR
img_bk =  paste0(mainDIR, "/img_bk/nio_logo5.png")

color_up = "blue"

myDF_xts = getXTS_tiingo(sym_in)



myDF_xts$ROC252 = ROC(myDF_xts$Close, 252)
myDF_xts$ROC252 = na.fill(myDF_xts$ROC252, 0)

nrowplot = 79
steps = 5

nrow_use = myDF_xts

myDF = myDF_xts
lastPrice = last(myDF$Close)
lastDate = as.character(index(myDF)[nrow(myDF)])
title1 = paste0('TSLA Price ' , lastDate)
subtitle1 = ""
#
i=1
img <- png::readPNG(img_bk)
rast <- grid::rasterGrob(img, interpolate = TRUE)
rast
i=1
multiplier = .5


steps_loop = as.integer((nrow(myDF)-nrowplot)/steps) +1
max(steps_loop)


for(i in 1: steps_loop){
  print(i)
  k = i * steps
  
  endStep = k + nrowplot
  maxBars = nrow(myDF)
  if(endStep > maxBars)
    endStep = maxBars
  
  xPlot = index(myDF)[endStep]
  saveFileName =paste0('/price_chart_out',i, ".png")
  yPlot = myDF$Close[endStep]
  lastYear = format(xPlot, '%m-%Y')
  ROC252_text = paste0(round(myDF$ROC252[endStep],3)*100, "%")
 
  title1 = paste0('$' ,sym_in_Text, ":  Annual Return -  " , lastYear, " || ", ROC252_text)

  p1 =    ggplot()+ 
    
    geom_line(data = myDF[1:endStep], aes(x = Index, y = Close), color ='black', size =2.75, lineend = 'round') + ##grey92  #grey77
    geom_line(data = myDF[1:endStep], aes(x = Index, y = Close, color=Close), size =2, lineend = 'round') + 
    annotation_custom(rast)+
    #geom_line(data = myDF[1:(nrowplot+ k)], aes(x = Index, y = Close, color = Index), size = 1) +  #lineend = "round"
    scale_colour_gradient(low="#EBBF1D", high=color_up) +  
    #scale_color_fivethirtyeight() +  
    theme_fivethirtyeight()+
    theme(#axis.title.x=element_blank(), 
      legend.position = 'none', 
      #      legend.key.width = unit(4, 'cm'), 
      #      legend.title = element_blank(),
      #      legend.text = element_blank(),
      title = element_text(size = 14)#,family= 'Avenir Next Condensed Medium')
    ) + 
    #ylab("Price - USD")+
    geom_point(aes(x = xPlot, y = yPlot), colour = "black", size =3)+
    geom_point(aes(x = xPlot, y = yPlot), colour = color_up, size =2)+

    labs(title=title1) 
  
  
  ggsave( paste0(saveDIR, saveFileName), p1, width = 9.283, height = 8.64, units = 'in', dpi = 200 )
  
  
}


