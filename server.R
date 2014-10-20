# DDP Project
# Author: ghostdatalearner
# Date: October 2014
# Code at https://github.com/ghostdatalearner/ddp_project
# Shiny app at https://ghostdatalearner.shinyapps.io/ddp_project/

library(shiny)
library(ggplot2)
library(plyr)
options(warn = -1)
# Read raw data and add DATE column
rawdata <- read.csv("unemployment_sp_2005_2014.csv",sep=";")
rawdata$DATE = as.Date(sprintf("%02d-%02d-01",rawdata$YEAR,rawdata$MONTH),format="%Y-%m-%d")
# Add data by gender for ratio calculations
dquotraw <- ddply(rawdata,.(DATE,GENDER),function(x) sum(x$UNEMPLOYED))
names(dquotraw) = c("DATE","GENDER","UNEMPLOYED")
# Create aux data frames by age group
djunior <- rawdata[rawdata$AGEGROUP=='LESS25',]
dsenior <- rawdata[rawdata$AGEGROUP=='EQMORE25',]
# Total evlution adding groups ages
dtotages <- djunior
for (i in 1:nrow(dtotages))
  dtotages[i,]$UNEMPLOYED <- dtotages[i,]$UNEMPLOYED  + dsenior[i,]$UNEMPLOYED
# Create aux data frames by gender
dmale <- rawdata[rawdata$GENDER=='M',]
dfemale <- rawdata[rawdata$GENDER=='F',]
dtotgend <- dmale
ud <- unique(dquotraw$DATE)
# Aux data frames for ratio and speed computation
dquotgend <- data.frame(rep(0,length(ud)),ud,rep(0,length(ud)),rep(0,length(ud)))
names(dquotgend) <- c("RATE","DATE","UNEMPLOYED", "SPEED")
for (i in 1:nrow(dtotgend))
  dtotgend[i,]$UNEMPLOYED <- dtotgend[i,]$UNEMPLOYED  + dfemale[i,]$UNEMPLOYED
for (i in 1:length(ud))
{
  dquotgend[i,]$DATE = as.Date(ud[i])
  dquotgend[i,]$RATE <- dquotraw[dquotraw$DATE == ud[i] & dquotraw$GENDER == 'F' ,]$UNEMPLOYED/dquotraw[dquotraw$DATE == ud[i] & dquotraw$GENDER == 'M', ]$UNEMPLOYED
  dquotgend[i,]$UNEMPLOYED <- dquotraw[dquotraw$DATE == ud[i] & dquotraw$GENDER == 'F' ,]$UNEMPLOYED + dquotraw[dquotraw$DATE == ud[i] & dquotraw$GENDER == 'M', ]$UNEMPLOYED
  if (i>1){
    dquotgend[i,]$SPEED <- 100*(dquotgend[i,]$UNEMPLOYED - dquotgend[i-1,]$UNEMPLOYED)/dquotgend[i-1,]$UNEMPLOYED
  }
}
dquotgend[1,]$SPEED <- 0

# Subset input data frame from initial(liminf) to final(limsup) years
subset_date <- function(m,liminf, limsup)
{
  datelimits <- c(paste0(liminf,"-01-01"), paste0(limsup,"-12-31"))
  m <- m[as.POSIXct(m$DATE)>=as.POSIXct(datelimits[1]) & as.POSIXct(m$DATE)<=as.POSIXct(datelimits[2]),]
}

# Paint time series evolution plot. Valid for Gender and Age plots.
paintplot <- function(auxtot,xlab="",ylab="",title="",factor="",colors = "", names = "")
{
  pl_abs <- ggplot(auxtot) + labs( x = xlab, y = ylab)  + ylim(0,1.05*max(auxtot$UNEMPLOYED)/1000)
  if (factor == "GENDER")
    pl_abs <- pl_abs + geom_line(aes(x=DATE,y=UNEMPLOYED/1000,color = GENDER))
  if (factor == "AGE GROUP")
    pl_abs <- pl_abs + geom_line(aes(x=DATE,y=UNEMPLOYED/1000,color = AGEGROUP)) 
  if (length(factor) == 0)
    pl_abs <- pl_abs + geom_line(aes(x=DATE,y=UNEMPLOYED))
  if (length(colors)>0 & length(names) >0)
    pl_abs <- pl_abs + theme_bw() + scale_color_manual(values=colors , labels = names)
  pl_abs <- pl_abs + ggtitle(title) 
  pl_abs <- pl_abs + theme(axis.title.y = element_text(vjust=1.3)) + theme(axis.title.x = element_text(vjust=-0.3)) + theme(title = element_text(vjust=1.3))
  pl_abs <- pl_abs + theme(legend.position="top",legend.direction="horizontal")+guides(color=guide_legend(ncol=2))
  return(pl_abs)
}

# Paint the F/M ratio graph
paintquot <- function(k,xlab="",ylab="",title="")
{
  pl_abs <- ggplot(k) + labs( x = xlab, y = ylab)  + theme_bw() + theme(legend.position="none")
  pl_abs <- pl_abs + geom_line(aes(x=DATE,y=RATE,color = 'red'))
  pl_abs <- pl_abs + ggtitle(title) 
  pl_abs <- pl_abs + theme(axis.title.y = element_text(vjust=1.3)) + theme(axis.title.x = element_text(vjust=-0.3)) + theme(title = element_text(vjust=1.3))
  pl_abs <- pl_abs + geom_text(data = k, mapping=aes(x=DATE[as.integer(0.7*length(DATE))], y=min(RATE)+0.5*(max(RATE)-min(RATE))), size=4,label = minmaxdates(k,which(names(k)=="RATE"),twodecs=TRUE))
  return(pl_abs)
}

# Paint the Speed Graph
paintspeed <- function(k,xlab="",ylab="",title="")
{
  #yvmax = 1.1*max(max(k$SPEED),abs(min(k$SPEED)))
  k$color<- ifelse(k$SPEED < 0, "red", "green") 
  pl_abs <- ggplot(k,aes(DATE,SPEED))+geom_bar(stat="identity",aes(fill = color)) + ggtitle(title)
  pl_abs <- pl_abs + labs( x = xlab, y = ylab)  + theme_bw() + theme(legend.position="none")#+ ylim(-yvmax,yvmax)
  pl_abs <- pl_abs + theme(axis.title.y = element_text(vjust=1.3)) + theme(axis.title.x = element_text(vjust=-0.3)) + theme(title = element_text(vjust=1.3))
  pl_abs <- pl_abs + geom_text(data = k, mapping=aes(x=DATE[as.integer(0.5*length(DATE))], y=-3.5), size=4,label = minmaxdates(k,which(names(k)=="SPEED"),twodecs=TRUE))
  return(pl_abs)
}

# Process time evolution by Gender
processgendevolution <- function (k, gender=c("TOTAL"))
{
  if (length(gender)==0)
    gender = c("TOTAL")
  colores <- c()
  nombres <- c()
  # Create empty data frame for Total series if chosen
  auxtot <- data.frame(t(rep(NA,length(names(k)))))[numeric(0), ]
  names(auxtot) <- names(k)
  levels(auxtot$GENDER) <- c("F","M","All")
  # Subset by Gender
  a <- k[k$GENDER=='F',]
  b <- k[k$GENDER=='M',]
  # Prepare colors and legend texts
  if ("F" %in% gender){
    colores <- c(colores, "red")
    nombres <- c(nombres, paste("Female\n",minmaxdates(a,which(names(a)=="UNEMPLOYED"))) )
    auxtot <- rbind(auxtot,a)
  }
  if ("M" %in% gender){
    colores <- c(colores, "blue")
    nombres <- c(nombres, paste("Male\n",minmaxdates(b,which(names(b)=="UNEMPLOYED"))) )
    auxtot <- rbind(auxtot,b)
  }
  if ("TOTAL" %in% gender)
  {
    colores <- c(colores, "black")
    suma <- ddply(k,.(DATE),function(x) sum(x$UNEMPLOYED))
    names(suma) <- c("DATE","UNEMPLOYED")
    alldf <- a
    alldf$GENDER <- 'All'
    alldf$UNEMPLOYED <- suma$UNEMPLOYED
    alldf$DATE <- suma$DATE
    alldf$AGEGROUP <- 'All'
    alldf$MONTH <- 0
    alldf$YEAR <- 0
    auxtot <- rbind(auxtot,alldf)
    nombres <- c(nombres, paste("TOTAL\n",minmaxdates(auxtot[auxtot$GENDER == 'All',],which(names(auxtot)=="UNEMPLOYED"))) )
  }
  # Return the ggplot2 graph
  return(paintplot(auxtot,xlab = "Date", ylab = "Unemployed (thousands)", 
                   title = "Unemployment Evolution by Gender", 
                   factor ="GENDER", colors = colores,names = nombres)  )
}


# Find minimum and maximum values and dates 
minmaxdates <- function(k,col,twodecs=FALSE)
{
  minimum <- min(k[col])
  datemin <- k[which(k[col] == minimum),]$DATE
  maximum <- max(k[col])
  datemax <- k[which(k[col] == maximum),]$DATE
  if (twodecs >0){
    minimum <- sprintf("%0.3f",minimum)
    maximum <- sprintf("%0.3f",maximum)
  }
  
  return(paste0("\n  Max: ",maximum," (",datemax,")\n","  Min: ",minimum," (",datemin,")\n"))
}

# Equivalent to Gender Evolution for Age Group
processageevolution <- function (k, agegroup=c("TOTAL"))
{
  if (length(agegroup)==0)
    agegroup = c("TOTAL")
  colores <- c()
  nombres <- c()
  auxtot <- data.frame(t(rep(NA,length(names(k)))))[numeric(0), ]
  names(auxtot) <- names(k)
  levels(auxtot$AGEGROUP) <- c("EQMORE25","LESS25","All")
  a <- k[k$AGEGROUP=='EQMORE25',]
  b <- k[k$AGEGROUP=='LESS25',]
  if ("A" %in% agegroup){
    colores <- c(colores, "green")
    nombres <- c(nombres, paste("Adult\n",minmaxdates(a,which(names(a)=="UNEMPLOYED"))) )
    auxtot <- rbind(auxtot,a)
  }
  if ("Y" %in% agegroup){
    colores <- c(colores, "magenta")
    nombres <- c(nombres, paste("Young < 25yrs \n",minmaxdates(b,which(names(b)=="UNEMPLOYED"))) )
    auxtot <- rbind(auxtot,b)
  }
  if ("TOTAL" %in% agegroup)
  {
    colores <- c(colores, "black")
    suma <- ddply(k,.(DATE),function(x) sum(x$UNEMPLOYED))
    names(suma) <- c("DATE","UNEMPLOYED")
    alldf <- a
    alldf$GENDER <- 'All'
    alldf$UNEMPLOYED <- suma$UNEMPLOYED
    alldf$DATE <- suma$DATE
    alldf$AGEGROUP <- 'All'
    alldf$MONTH <- 0
    alldf$YEAR <- 0
    auxtot <- rbind(auxtot,alldf)
    nombres <- c(nombres, paste("TOTAL \n",minmaxdates(auxtot[auxtot$AGEGROUP=='All',],which(names(auxtot)=="UNEMPLOYED"))) )
  }
  return(paintplot(auxtot,xlab = "Date", ylab = "Unemployed (thousands)", 
                   title = "Unemployment Evolution by Age Group", 
                   factor ="AGE GROUP", colors = colores, names = nombres)  )
}

# A Blank graph is generated for Gender or/and Age if none of Gender/Age options are checked
WarningGraph <- function(Message)
{
  text = paste(Message)
    ggplot() + 
    annotate("text", x = 2, y = 5, size=8, label = text) + 
    theme_bw()+
    theme(panel.grid.major=element_blank(),axis.text.x=element_text(size=0,color="white",
                                                                    lineheight=0,vjust=1), 
          axis.text.y=element_text(size=0,color="white", lineheight=0,hjust=1),
          axis.title.x=element_text(size=0,color="white",vjust=1), 
          axis.title.y=element_text(size=0,color="white",vjust=0.5), 
          axis.ticks=element_line(color="white",size = 0),
          panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
}


# Server function
shinyServer(function(input, output) {
  # Just for testing
  # output$range <- renderText({input$slideryears})
  
  # Reactive Gender plot generation
  output$gendPlot <- renderPlot({
    if (length(input$checkGender) == 0)
      plot1 <- WarningGraph("Choose at least one Gender option, please")
    else{
      datosplot <- subset_date(dtotages, input$slideryears[1], input$slideryears[2])
      plot1 <- processgendevolution(datosplot, gender=input$checkGender)
    }
    print(plot1)
  })

  # Reactive Age plot generation
  output$agePlot <- renderPlot({
    if (length(input$checkAge) == 0)
      plot2 <- WarningGraph("Choose at least one Age Group, please")
    else{
      datosplot <- subset_date(dtotgend, input$slideryears[1], input$slideryears[2])
      plot2 <- processageevolution(datosplot, agegroup=input$checkAge)
    }
    print(plot2)
  })

  # Reactive F/M Ratio plot generation
  output$quotPlot <- renderPlot({
    datosquot <- subset_date(dquotgend, input$slideryears[1], input$slideryears[2])
    plot3 <- paintquot(datosquot, xlab = "Date", ylab = "Ratio", title = "Female/Male Unemployment Ratio")
    print(plot3)
  })

  # Reactive Speed plot generation
  output$speedPlot <- renderPlot({
    datosspeed <- subset_date(dquotgend, input$slideryears[1], input$slideryears[2])
    plot4 <- paintspeed(datosspeed, xlab = "Date", ylab = "Rate", title = "Unemploment monthly growth rate (%)")
    print(plot4)
  })

})