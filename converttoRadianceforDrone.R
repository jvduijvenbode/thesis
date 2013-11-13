###############################################################################
### small script to read the txt output files from the goniometer ASD
### radiance measurements will be extracted (column 3)
### Requirements:
### - There should be no other files in the folder than the measurements!
###
###   Harm Bartholomeus           31-03-2011
###   harm.bartholomeus@wur.nl
###   Wageningen University - Centre for Geo-Information
###
###
###   Any comments are welcome, including solutions for my lousy scripting...
###############################################################################

### Put all raw data in a folder named "input"
### create a folder named "output" as well

setwd("D:/ASD conversion/measurements/field flight")

cal_file<-read.csv("D:/ASD conversion/cal/cal6395.csv")

dark_current <-read.table("D:/ASD conversion/cal/130207024417_0 _0 _400_90   Dark.txt") 

dark_current<-dark_current[,c(2)]

#####################################################################
### SETTINGS!!!
### set the right integration times here, as they are coming from the optimisation measurement opposite the lamp

itime=3
gain1=16
gain2=16

## select the right foreoptic option
#inst_DN <- cal_file$X1i161721.raw[1:2150] ## for 1 degree foreoptic
#inst_DN <- cal_file$X8i161721.raw ## for 8 degree foreoptic
#inst_DN <- cal_file$XNi161721.raw ## for no foreoptic
inst_DN <- cal_file$Cos63953.raw[1:2151] ## for cosine receptor

head(cal_file)

### End settings
####################################################################

### Reading and plotting calibration data ASD

panel<-cal_file$BSE63953.ref[1:2151] ## reflectance of panel during ASD calibration
irr<-cal_file$LMP63953.ill[1:2151]   ## irradiance of lamp used during ASD calibration

wavelength <- c(350:2500) ## create vector named wavelength, for plotting. 

## create plots in output folder
# png("../../output/Cal_DN.png")
# plot(inst_DN~wavelength, type="l", main="DN from ASD during calibration", xlab="Wavelength [nm]", ylab="DN")
# dev.off()
# 
# png("../../output/Cal_Panel_reflectance.png")
# plot(panel~wavelength, type="l", main="Panel used for ASD calibration", ylab="Reflectance [-]", xlab="Wavelength [nm]")
# dev.off()
# 
# png("../../output/Cal_lamp_irradiance.png")
# plot(irr, type="l", main="Irradiance lamp used for ASD calibration", xlab="Wavelength [nm]", ylab="Irradiance [W/m2/sr]")
# dev.off()

####################################################################
### Reading all the raw measurements in "input" folder
### Converting the Digital Numbers to Radiance values
### Plotting a graph for each individual measurement
### Create an ouput file with all the Radiance spectra in it
####################################################################


#data <- read.csv("E:/Documenten/cursussen/thesis2013/jonas_v_duijvenbode/ASD conversion/measurements/12-09 VaG.csv", header=TRUE)

#head(data)

#plot (unlist(data[1])~unlist(data[2]))



a<-list.files()                           # list all files in the "input" dir

wavelength<-data.frame(wavelength=c(400:1000)) # wavelength range ASD fieldspec
spec<-wavelength

## start spectrum read
for (x in a) {
  
  
  spectrum<-read.table(x, skip=1, sep="\t")
  
  
  head(spectrum)
  
  #  plot(spectrum[,c(2)])
  
  names(spectrum)<- c("wavelength", x)
  DN<-as.numeric(spectrum[,c(2)])
  # DN<- as.numeric(DN)- as.numeric(dark_current)
  DN <-cbind(spec[,c(1)], DN, panel, irr, inst_DN)
  
  ## apply different corrections for the different units of the ASD 
  
  sub1 <-as.data.frame(DN[1:651,])
  sub2 <- as.data.frame(DN[652:1481,])
  sub3<- as.data.frame(DN[1481:2150,])
  
  RAD1<- as.numeric(sub1$panel*sub1$irr*sub1$DN*(2^itime*17))/(pi*sub1$inst_DN*136)
  RAD2<- as.numeric(sub2$panel*sub2$irr*sub2$DN*(1/gain1))/(pi*sub2$inst_DN*(1/16))
  RAD3<- as.numeric(sub3$panel*sub3$irr*sub3$DN*(1/gain2))/(pi*sub3$inst_DN*(1/16))                                      
  
  RAD <- c(RAD1, RAD2, RAD3) 
  
  #spectrum <- (spectrum[,c(3)])
  name <- x
  
  length(RAD)
  length(wavelength)
  
  ### create a plot for every single measurement 
  # dir.create("../../output/Radiance_plots/", showWarnings = FALSE)
  # png(paste("../../output/Radiance_plots/",name,"Cal_Panel_reflectance.png", sep="")) 
  # plot(RAD~unlist(wavelength), type="l", ylim=c(-0.2,0.7), main=x, ylab="Spectral Irradiance [w/m2/nm]", xlab="Wavelength [nm]")
  # dev.off() ## end plot
  
  spec<- cbind(spec, `colnames<-`(cbind(RAD[51:651]), name))
}
spec=spec[1:3000]

write.csv(spec, "../../output/irradiance_spectra_fieldflightshortened.csv")


#############################################################################
###                       End of script so far                            ###
#############################################################################
