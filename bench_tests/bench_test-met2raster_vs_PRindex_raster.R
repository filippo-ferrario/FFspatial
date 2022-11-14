library(sf)
library(spatstat)

source('C:/Users/ferrariof/Documents/GitHub/FFspatial/R/met2Raster.R')
source('C:/Users/ferrariof/Documents/GitHub/FFspatial/R/PRindex_raster.R')
source('C:/Users/ferrariof/Documents/GitHub/FFspatial/R/PRindex.R')



trssf<-sf::st_read('C:/Users/ferrariof/Documents/DFO-Godbout-2020-localSDM/GIS/godbout-fall2020.gpkg', layer='transects')
sbs_vect<-sf::st_read('C:/Users/ferrariof/Documents/DFO-Godbout-2020-localSDM//GIS/godbout-fall2020.gpkg', layer='segmentation_refined')
rcksf<-sf::st_cast(sbs_vect[sbs_vect$class_corrected=='rock',], 'POLYGON')


new<-sf::st_buffer(sf::st_centroid(trssf[1,]), dist=2)
n<-5
res<-data.frame(time=NA,fun=rep(c('met','ras'),each =n))

for (i in 1:n){
ras<-system.time(
	mt1<-PRindex_raster(obs_polygon=new, side.cell=0.1,poly=rcksf, buffer=1, zero_distance=0.01) 
)

met<-system.time(
	r1<-met2raster(obs_polygon=new, side.cell=0.1, PRindex,make_spatstat_im=TRUE,poly=rcksf, buffer=1, zero_distance=0.01) 
)

res[c(i,i+n),]$time<-c(met[3],ras[3])

}

# res$fun<-as.factor(rep(c('met','ras'),each =4))
res$fun<-as.factor(res$fun)
tapply(res$time, res$fun, mean)
tapply(res$time, res$fun, sd)/sqrt(n)
t.test(res$time[1:n],res$time[(n+1):nrow(res)])

boxplot(res$time[1:n],res$time[(n+1):nrow(res)])

raster::plot(mt1)
windows()
raster::plot(r1)