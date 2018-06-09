#server.R
library(shiny)
library(ggplot2)
library(data.table)
library(reshape2)
library(Matching)

shinyServer(function(input, output) {
  
  # Reactive expression to compose a data frame containing all of the values
  PO <- reactive({
    
    # Compose data frame
    df1 = data.frame(
      Shape=c("D","S","D","S","D","S","D","S"),
      Color=c("R","R","G","G","R","R","G","G"),
      Grp = c(1,2,3,4,1,2,3,4),
      Trt=c(0,0,0,0,1,1,1,1),
      TrtLabel=factor(rep(c("Not treated","Treated"),each=4)),
      
      Value = c(input$RD,
                input$RS,
                input$GD,
                input$GS,
                input$RD_effect,
                input$RS_effect,
                input$GD_effect,
                input$GS_effect
                ),
      
      stringsAsFactors=FALSE)
        
    df1[5:8,6] <- df1[1:4,6] + df1[5:8,6]
    df1
    
  })
  
probValues <- reactive({
     
  data.frame(Shape=c("D","S","D","S"),
             Color=c("R","R","G","G"),
             Grp = c(1,2,3,4),
             Value=c(input$RD_prob,input$RS_prob,input$GD_prob,input$GS_prob))
})

simValues  <- reactive({
  n = 250 # not as well balanced for n=250
  
  df.param = data.table(PO(), key=c("Color","Shape"))
  
  Color = sample(c("R","G"),n,replace=TRUE)
  Shape = sample(c("D","S"),n,replace=TRUE)

  df = data.table(id=1:n,Color,Shape,key=c("Color","Shape"))

  df.1 = df[df.param[Trt==1]]
  setnames(df.1,"Value","mu.Y1")
  df.1[,Trt:=NULL]

  df.0 = df[df.param[Trt==0]]
  df.0 = df.0[,list(id,mu.Y0=Value)]

  setkey(df.0,id)
  setkey(df.1,id)

  df.mu = df.1[df.0]
  setkey(df.mu,Grp)

  pvalues = data.table(probValues(),key="Grp")  
  pvalues = pvalues[,list(Grp,p=Value)]
  
  dt = df.mu[pvalues]
  setkey(dt,id)
  
  
  dt[,Trt:=rbinom(n,1,p)]
  dt[,Y0:=round(rnorm(n,mu.Y0,2),1)]
  dt[,Y1:=round(rnorm(n,mu.Y1,2),1)]
  dt[,Y:=Trt*Y1+(1-Trt)*Y0]

  dt
})

randomValues  <- reactive({
  n = 250 # not as well balanced for n=250
  
  df.param = data.table(PO(), key=c("Color","Shape"))
  
  Color = sample(c("R","G"),n,replace=TRUE)
  Shape = sample(c("D","S"),n,replace=TRUE)
  
  df = data.table(id=1:n,Color,Shape,key=c("Color","Shape"))
  
  df.1 = df[df.param[Trt==1]]
  setnames(df.1,"Value","mu.Y1")
  df.1[,Trt:=NULL]
  
  df.0 = df[df.param[Trt==0]]
  df.0 = df.0[,list(id,mu.Y0=Value)]
  
  setkey(df.0,id)
  setkey(df.1,id)
  
  df.mu = df.1[df.0]
  setkey(df.mu,Grp)
  
  pvalues = data.table(probValues(),key="Grp")  
  pvalues = pvalues[,list(Grp,p=Value)]
  
  dt = df.mu[pvalues]
  setkey(dt,id)
  
  
  dt[,Trt:=rbinom(n,1,.5)]
  dt[,Y0:=round(rnorm(n,mu.Y0,2),1)]
  dt[,Y1:=round(rnorm(n,mu.Y1,2),1)]
  dt[,Y:=Trt*Y1+(1-Trt)*Y0]
  
  dt
})

showCausal <- reactive({
   data.frame(Name=c("CE","OE"),
              Value=c(input$trueCausal,input$observed)
   )
})

withReplace <- reactive({
   input$replace
})

showCausalmatch <- reactive({
  data.frame(Name=c("ACE","ATT","OBS"),
             Value=c(input$ACE,input$ATT,input$OBS)
  )
})

output$simPlot <- renderPlot({
  
  dt = simValues()

  dt.melt <- melt(dt,
                id.vars=c("id","Trt","Color","Shape"),
                measure.vars=c("Y0","Y1"),
                variable.factor=TRUE)

  dt.melt[Trt==0 & variable=="Y0",Y:=value]
  dt.melt[Trt==1 & variable=="Y1",Y:=value]
  
  dt.melt[variable=="Y0",varlab:="Not treated"]
  dt.melt[variable=="Y1",varlab:="Treated"]

  setkey(dt.melt,id)
  
  miny = dt.melt[!is.na(Y),min(Y)]
  maxy = dt.melt[!is.na(Y),max(Y)]

  dt.mean = data.table(id = c(1,1),
                       value=c(dt[Trt==0,mean(Y)], 
                               dt[Trt==1,mean(Y)]),
                       variable=c("Not treated","Treated")
  )
  
  dt.meanexp = data.table(id = c(1,1),
                       value=c(dt[,mean(Y0)], 
                               dt[,mean(Y1)]),
                       variable=c("Not treated","Treated")
  )
  
  set.seed(124)
  
  dt.mean1 = data.table(id = 1,
                       effect = "Observed effect",
                       value=c(dt[Trt==1,mean(Y)] - dt[Trt==0,mean(Y)])
  )
  
  dt.meanexp1 = data.table(id = 0,
                          effect = "True causal effect",
                          value=c(dt[,mean(Y1)]- dt[,mean(Y0)])
  )
  
  dt.means = rbind(dt.meanexp1,dt.mean1)
  bias = dt.means[id==1,value] - dt.means[id==0,value]
  
  biastext = paste0("(bias = ",round(bias,1),")")

  
  #ymin = min(-20,dt.means[,min(value)])
  #ymax = max(20,dt.means[,max(value)]+10)

  p <- ggplot(aes(x=varlab,y=value,group=id),data=dt.melt) + 
    geom_line(aes(color=Color,lty=Shape),size=.5,alpha=.3) +
    geom_jitter(aes(x=varlab,y=Y),size=1.5,color="grey25",
              position=position_jitter(w = 0.04, h = 0.0)) + 
    scale_colour_manual(values = c("darkolivegreen4","orangered4")) +
    scale_linetype_manual(values=c("dotted","solid")) +
    ylim(miny - 10, maxy + 10) +
    theme(legend.position="none",
          axis.title.x=element_blank(),
          plot.title = element_text(lineheight=.8, face="bold",hjust=0)) +
    ylab("Potential outcome") +
    ggtitle(paste0("Confounding ",biastext))
    
  
  if (showCausal()$Value[1]==TRUE) {
    p <- p + geom_line(aes(x=variable,y=value,group=id),data=dt.meanexp,size=1.5,lty=3) 
  }
  
  if (showCausal()$Value[2]==TRUE) {
    p <- p + geom_line(aes(x=variable,y=value,group=id),data=dt.mean,size=2) 
  }
  
  p
  
    
})  

output$randomPlot <- renderPlot({
  
  dt = randomValues()
  
  dt.melt <- melt(dt,
                  id.vars=c("id","Trt","Color","Shape"),
                  measure.vars=c("Y0","Y1"),
                  variable.factor=TRUE)
  
  dt.melt[Trt==0 & variable=="Y0",Y:=value]
  dt.melt[Trt==1 & variable=="Y1",Y:=value]
  
  dt.melt[variable=="Y0",varlab:="Not treated"]
  dt.melt[variable=="Y1",varlab:="Treated"]
  
  setkey(dt.melt,id)
  
  miny = dt.melt[!is.na(Y),min(Y)]
  maxy = dt.melt[!is.na(Y),max(Y)]
  
  dt.mean = data.table(id = c(1,1),
                       value=c(dt[Trt==0,mean(Y)], 
                               dt[Trt==1,mean(Y)]),
                       variable=c("Not treated","Treated")
  )
  
  dt.meanexp = data.table(id = c(1,1),
                          value=c(dt[,mean(Y0)], 
                                  dt[,mean(Y1)]),
                          variable=c("Not treated","Treated")
  )
  
  set.seed(124)
  
  p <- ggplot(aes(x=varlab,y=value,group=id),data=dt.melt) + 
    geom_line(aes(color=Color,lty=Shape),size=.5,alpha=.3) +
    geom_jitter(aes(x=varlab,y=Y),size=1.5,color="grey25",
                position=position_jitter(w = 0.04, h = 0.0)) + 
    scale_colour_manual(values = c("darkolivegreen4","orangered4")) +
    scale_linetype_manual(values=c("dotted","solid")) +
    ylim(miny - 10, maxy + 10) +
    theme(legend.position="none",
          axis.title.x=element_blank(),
          plot.title = element_text(lineheight=.8, face="bold",hjust=0)) +
    ylab("Potential outcome") +
    ggtitle("No confounding")
  
  
  if (showCausal()$Value[1]==TRUE) {
    p <- p + geom_line(aes(x=variable,y=value,group=id),data=dt.meanexp,size=1.5,lty=3) 
  }
  
  if (showCausal()$Value[2]==TRUE) {
    p <- p + geom_line(aes(x=variable,y=value,group=id),data=dt.mean,size=2) 
  }
  
  p
  
  
})  

output$matchPlot <- renderPlot({
  
  dt = simValues()
  
  
  # Matching
  
  TR = dt$Trt
  X = with(dt,matrix(c(factor(Color),factor(Shape)),nrow=length(TR)))
  
  # Matching with replacement
  if (withReplace()==TRUE)
    matches <- Match(Y=NULL,Tr=TR,X=X,ties=FALSE,exact=TRUE,replace=TRUE)
  else
    matches <- Match(Y=NULL,Tr=TR,X=X,ties=FALSE,exact=TRUE,replace=FALSE)
  
  
  
  dt.match = dt[c(matches$index.treated,matches$index.control),]
  
  dt.melt <- melt(dt.match,
                  id.vars=c("id","Trt","Color","Shape"),
                  measure.vars=c("Y0","Y1"),
                  variable.factor=TRUE)
  
  dt.melt[Trt==0 & variable=="Y0",Y:=value]
  dt.melt[Trt==1 & variable=="Y1",Y:=value]
  
  dt.melt[variable=="Y0",varlab:="Not treated"]
  dt.melt[variable=="Y1",varlab:="Treated"]
  
  setkey(dt.melt,id)
  
  miny = dt.melt[!is.na(Y),min(Y)]
  maxy = dt.melt[!is.na(Y),max(Y)]
  
  # True causal effect
  
  dt.meanexp = data.table(id = c(1,1),
                          value=c(dt[,mean(Y0)], 
                                  dt[,mean(Y1)]),
                          variable=c("Not treated","Treated")
  )
  
  dt.mean.match = data.table(id = c(1,1),
                       value=c(dt.match[Trt==0,mean(Y)], 
                               dt.match[Trt==1,mean(Y)]),
                       variable=c("Not treated","Treated")
  )
  
  dt.meanexp.match = data.table(id = c(1,1),
                          value=c(dt[Trt==1,mean(Y0)], 
                                  dt[Trt==1,mean(Y1)]),
                          variable=c("Not treated","Treated")
  )
  
  set.seed(124)
  
  p <- ggplot(aes(x=varlab,y=value,group=id),data=dt.melt) + 
    geom_line(aes(color=Color,lty=Shape),size=.5,alpha=.3) +
    geom_jitter(aes(x=varlab,y=Y),size=1.5,color="grey25",
                position=position_jitter(w = 0.04, h = 0.0)) + 
    scale_colour_manual(values = c("darkolivegreen4","orangered4")) +
    scale_linetype_manual(values=c("dotted","solid")) +
    ylim(miny - 10, maxy + 10) +
    theme(legend.position="none",
          axis.title.x=element_blank(),
          plot.title = element_text(lineheight=.8, face="bold",hjust=0)) +
    ylab("Potential outcome") +
    ggtitle(paste0("Matching (estimate ATT)"))
  
   if (showCausalmatch()$Value[1]==TRUE) {
     p <- p + geom_line(aes(x=variable,y=value,group=id),data=dt.meanexp,size=1.5,lty=3,color="red") 
   }
  
   if (showCausalmatch()$Value[2]==TRUE) {
     p <- p + geom_line(aes(x=variable,y=value,group=id),data=dt.meanexp.match,size=1.5,lty=3) 
   }
   
   if (showCausalmatch()$Value[3]==TRUE) {
     p <- p + geom_line(aes(x=variable,y=value,group=id),data=dt.mean.match,size=2) 
   }

 

  
  p
  
  
})  

 output$effectPlot <- renderPlot({

     df = PO()
     ymin = min(df$Value - 10)
     ymax = max(df$Value + 10)


     ggplot(df,aes(x=factor(TrtLabel),y=Value,group=Grp)) + 
       geom_point() + 
       geom_line(aes(color=Color,lty=Shape),size=1.2) +
       ylim(ymin,ymax) +
       ylab("Potential outcome") +
       xlab("Treatment status") +
       scale_color_manual(values=c("darkolivegreen4","orangered4")) +
       scale_linetype_manual(values=c("dotted","solid")) +
       theme(legend.position="none",
             axis.title.x=element_blank())
   })
 
 output$probPlot <- renderPlot({
   
   df = probValues()
   
   ggplot(data=df,aes(x=factor(Grp),y=Value)) + 
     geom_bar(aes(fill=factor(Color),linetype=factor(Shape)),
              stat = "identity",color="black",size=1,alpha=.5) + 
     ylim(0,1) +
     ylab("Probability of treatment") +
     xlab("Covariate Group") +
     scale_fill_manual(values=c("darkolivegreen4","orangered4")) +
     scale_linetype_manual(values=c("dotted","solid")) +
     theme(axis.ticks.x=element_blank(),
           axis.text.x = element_blank(),
           legend.position="none")
 })

output$effectDiff <- renderPlot({
  
  dt = simValues()
  
  dt.mean = data.table(id = 1,
                       effect = "Observed effect",
                       value=c(dt[Trt==1,mean(Y)] - dt[Trt==0,mean(Y)])
  )
  
  dt.meanexp = data.table(id = 0,
                          effect = "True causal effect",
                          value=c(dt[,mean(Y1)]- dt[,mean(Y0)])
  )
  

  dt.means = rbind(dt.meanexp,dt.mean)
  bias = dt.means[id==1,value] - dt.means[id==0,value]
  
  biastext = paste0("Bias: ",round(bias,1))
  dt.bias = data.table(x=1.5,y=17.5,biastext)
  
  ymin = min(-20,dt.means[,min(value)])
  ymax = max(20,dt.means[,max(value)]+10)
  
  ggplot(data=dt.means,aes(x=factor(id),y=value)) + 
    geom_bar(stat = "identity",fill="black",alpha=.5,width=.5) + 
    geom_text(aes(label=effect),vjust=-1) +
    geom_text(aes(x=1.5,y=17.5,label=biastext),data=dt.bias) +
    ylab("Treatment effect") +
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x = element_blank()
    ) +
    scale_y_continuous(limits=c(ymin,ymax))
})


output$simTable <- renderDataTable(
  simValues(),
  options = list(iDisplayLength = 10,bFilter = FALSE)
)
  
})