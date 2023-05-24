# FUNCTIONS ------
metabolism.size.sim<-function(data.sim, 
                              b.smr,
                              a.smr,
                              b.mmr,
                              a.mmr,
                              data = NULL,
                              plot = FALSE){

  # sanity check, same results
  data.sim$pred.mmr.mgO2min<-exp(a.mmr)*(data.sim$BW_kg^b.mmr) # prediction in power equation form
  # data.sim$pred.mmr.mgO2min<-exp(a.mmr+(log(data.sim$BW_kg)*b.mmr)) # prediction in linear equation form 
  data.sim$pred.mmr.mgO2minKG<-(exp(a.mmr)*(data.sim$BW_kg^b.mmr))/(data.sim$BW_kg)
  data.sim$pred.smr.mgO2min<-exp(a.smr)*(data.sim$BW_kg^b.smr)
  data.sim$pred.smr.mgO2minKG<-(exp(a.smr)*(data.sim$BW_kg^b.smr))/(data.sim$BW_kg)
  
  data.sim$pred.as.mgO2min<-data.sim$pred.mmr.mgO2min - data.sim$pred.smr.mgO2min
  data.sim$pred.as.mgO2minKG<-data.sim$pred.mmr.mgO2minKG - data.sim$pred.smr.mgO2minKG

  data.sim$pred.fas.mgO2min<-data.sim$pred.mmr.mgO2min / data.sim$pred.smr.mgO2min
  
  if(any(data.sim$pred.as.mgO2min<0)){
    message("Negative AS")
    data.sim<-data.sim[-c(which(data.sim$pred.as.mgO2min < 0)),]
  }
  
  b.as<-round(
        coef(lm(log(data.sim$pred.as.mgO2min) ~ log(data.sim$BW_kg)))[2],3)
  a.as<-round(
        coef(lm(log(data.sim$pred.as.mgO2min) ~ log(data.sim$BW_kg)))[1],3)
  b.fas<-round(
        coef(lm(log(data.sim$pred.fas.mgO2min) ~ log(data.sim$BW_kg)))[2],3)
  a.fas<-round(
        coef(lm(log(data.sim$pred.fas.mgO2min) ~ log(data.sim$BW_kg)))[1], 3)
  
  data.sim$slope.SMR<-b.smr
  data.sim$int.SMR<-a.smr
  data.sim$slope.MMR<-b.mmr
  data.sim$int.MMR<-a.mmr
  data.sim$slope.AS<-b.as
  data.sim$int.AS<-a.as
  data.sim$slope.FAS<-b.fas
  data.sim$int.FAS<-a.fas
  
  
  if(!is.null(data)){
    data.sim$mmr.mgO2minKG.mass.correct<-((a.mmr*(data.sim$BW_kg^b.mmr))/(data.sim$BW_kg))+data.sim$residMMR
    data.sim$smr.mgO2minKG.mass.correct<-((a.smr*(data.sim$BW_kg^b.smr))/(data.sim$BW_kg))+data.sim$residSMR
    # data.sim$as.mgO2minKG.mass.correct<-((a.as*(data.sim$BW_kg^b.as))/(data.sim$BW_kg))+data.sim$residAS
  }
 
  if(plot){
    ggplot(data.sim, aes(BW_kg, pred.mmr.mgO2min))+
      geom_line()+
      geom_point(size = 1)+
      geom_line(aes(BW_kg, pred.smr.mgO2min), color = "grey50")+
      geom_point(aes(BW_kg, pred.smr.mgO2min), color = "grey50", size = 1)+
      geom_line(aes(BW_kg, pred.as.mgO2min), color = "darkgreen")+
      geom_point(aes(BW_kg, pred.as.mgO2min), color = "darkgreen", size = 1)+
      theme_light()
    
    ggplot(data.sim, aes(BW_kg, pred.mmr.mgO2minKG))+
      geom_line()+
      geom_point(size = 1)+
      geom_line(aes(BW_kg, pred.smr.mgO2minKG), color = "grey50")+
      geom_point(aes(BW_kg, pred.smr.mgO2minKG), color = "grey50", size = 1)+
      geom_line(aes(BW_kg, pred.as.mgO2minKG), color = "darkgreen")+
      geom_point(aes(BW_kg, pred.as.mgO2minKG), color = "darkgreen", size = 1)+
      theme_light()
    
    ggplot(data.sim, aes(BW_kg, pred.mmr.mgO2min/pred.smr.mgO2min))+
      geom_line(color = "darkred")+
      geom_point(color = "darkred", size = 1)+
      theme_light()
  }

  return(data.sim)
}

# the length of the sequence of SMR and MMR slopes must (should) be the same for this to work
simulation.loop<-function(color.group.vary,
                          slopeseq.mmr,
                          slopeseq.smr,
                          intseq.mmr = log(10),
                          intseq.smr = log(0.5),
                          data.sim0){
  
  for(i in 1:length(slopeseq.mmr)){ 

    data.sim1<-metabolism.size.sim(data.sim = data.sim0, 
                                   b.smr = slopeseq.smr[i], a.smr = intseq.smr,
                                   b.mmr = slopeseq.mmr[i] , a.mmr = intseq.mmr,
                                   data = NULL, plot = FALSE)
    
    if(i == 1){
      data.sim1$simID<-i
      data.simFULL<-data.sim1
      exists("data.simFULL")
      
    }else{
      data.sim1$simID<-i
      data.simFULL<-rbind(data.simFULL, data.sim1)
      exists("data.simFULL")
    }
    
    if(color.group.vary == "byMMR"){
      title <- ggdraw() + 
        draw_label(
          "Darker the color, lower the MMR slope",
          fontface = 'bold',
          x = 0,
          hjust = 0
        )
    }else{
      title <- ggdraw() + 
        draw_label(
          "Darker the color, lower the RMR slope",
          fontface = 'bold',
          x = 0,
          hjust = 0
        )
    }
    
    if(any(data.simFULL$pred.as.mgO2min < 0)){
      message("Negative AS")
    }
    
    if( i == length(slopeseq.mmr)){
      
      if(color.group.vary == "byMMR"){
        p.mmr<-ggplot(data.simFULL, aes(BW_kg, pred.mmr.mgO2min,
                                        color = data.simFULL$slope.MMR,
                                        group = data.simFULL$slope.MMR))+
          geom_line()+
          geom_point(size = 1)+
          scale_color_gradient(low = "black", high = "dodgerblue")+
          theme_light()+
          labs(x="Body mass (kg)", y=expression(MMR~mgO[`2`]*min^-1))+
          theme(legend.position = "none")
        p.mmrKG<-ggplot(data.simFULL, aes(BW_kg, pred.mmr.mgO2minKG,
                                          color = data.simFULL$slope.MMR,
                                          group = data.simFULL$slope.MMR))+
          geom_line()+
          geom_point(size = 1)+
          scale_color_gradient(low = "black", high = "dodgerblue")+
          theme_light()+
          labs(x="Body mass (kg)", y=expression(MMR~mgO[`2`]*min^-1*kg^-1))+
          theme(legend.position = "none")
        p.mmr.scale<-ggplot(data.simFULL, aes(x=log(BW_kg), y=log(pred.mmr.mgO2min),
                                              color = data.simFULL$slope.MMR,
                                              group = data.simFULL$slope.MMR ))+
          geom_smooth(method = "lm", se = FALSE)+
          scale_color_gradient(low = "black", high = "dodgerblue")+
          theme_light()+
          labs(x="log(Body mass (kg))", y=expression(log(MMR~mgO[`2`]*min^-1)))+
          theme(legend.position = "none")
        p.smr<-ggplot(data.simFULL, aes(BW_kg, pred.smr.mgO2min,
                                        color = data.simFULL$slope.MMR,
                                        group = data.simFULL$slope.MMR))+
          geom_point(size = 1)+
          geom_line()+
          scale_color_gradient(low = "black", high = "red")+
          theme_light()+
          labs(x="Body mass (kg)", y=expression(SMR~mgO[`2`]*min^-1))+
          theme(legend.position = "none")
        p.smrKG<-ggplot(data.simFULL, aes(BW_kg, pred.smr.mgO2minKG,
                                          color = data.simFULL$slope.MMR,
                                          group = data.simFULL$slope.MMR))+
          geom_point(size = 1)+
          geom_line()+
          scale_color_gradient(low = "black", high = "red")+
          theme_light()+
          labs(x="Body mass (kg)", y=expression(SMR~mgO[`2`]*min^-1*kg^-1))+
          theme(legend.position = "none")
        p.smr.scale<-ggplot(data.simFULL, aes(x=log(BW_kg), y=log(pred.smr.mgO2min),
                                              color = data.simFULL$slope.MMR,
                                              group = data.simFULL$slope.MMR))+
          geom_smooth(method = "lm")+
          theme_light()+
          scale_color_gradient(low = "black", high = "red")+
          labs(x="log(Body mass (kg))", y=expression(log(SMR~mgO[`2`]*min^-1)))+
          theme(legend.position = "none")
        p.as<-ggplot(data.simFULL, aes(BW_kg, pred.as.mgO2min,
                                       color = data.simFULL$slope.MMR,
                                       group = data.simFULL$slope.MMR))+
          geom_point(size = 1)+
          geom_line()+
          scale_color_gradient(high = "#4FFBDF", low = "black")+
          theme_light()+
          labs(x="Body mass (kg)", y=expression(AS~mgO[`2`]*min^-1))+
          theme(legend.position = "none")
        p.asKG<-ggplot(data.simFULL, aes(BW_kg, pred.mmr.mgO2minKG - pred.smr.mgO2minKG,
                                         color = data.simFULL$slope.MMR,
                                         group = data.simFULL$slope.MMR))+
          geom_point(size = 1)+
          geom_line()+
          scale_color_gradient(high = "#4FFBDF", low = "black")+
          theme_light()+
          labs(x="Body mass (kg)", y=expression(AS~mgO[`2`]*min^-1*kg^-1))+
          theme(legend.position = "none")
        p.as.scale<-ggplot(data.simFULL, aes(x=log(BW_kg), y=log(pred.as.mgO2min),
                                             color = data.simFULL$slope.MMR,
                                             group = data.simFULL$slope.MMR))+
          geom_smooth(method = "lm")+
          theme_light()+
          scale_color_gradient(low = "black", high = "#4FFBDF")+
          labs(x="log(Body mass (kg))", y=expression(log(AS~mgO[`2`]*min^-1)))+
          theme(legend.position = "none")
        p.fas<-ggplot(data.simFULL, aes(BW_kg, pred.fas.mgO2min,
                                        color = data.simFULL$slope.MMR,
                                        group = data.simFULL$slope.MMR))+
          geom_point(size = 1)+
          geom_line()+
          scale_color_gradient(high = "#FF8066", low = "black")+
          theme_light() +
          labs(x="Body mass (kg)", y=expression(FAS))+
          theme(legend.position = "none")
        p.fas.scale<-ggplot(data.simFULL, aes(x=log(BW_kg), y=log(pred.fas.mgO2min),
                                              color = data.simFULL$slope.MMR,
                                              group = data.simFULL$slope.MMR))+
          geom_smooth(method = "lm")+
          theme_light()+
          scale_color_gradient(low = "black", high = "#FF8066")+
          labs(x="log(Body mass (kg))", y=expression(log(FAS)))+
          theme(legend.position = "none")
      }
      
      if(color.group.vary == "bySMR"){
        p.mmr<-ggplot(data.simFULL, aes(BW_kg, pred.mmr.mgO2min,
                                        color = data.simFULL$slope.SMR,
                                        group = data.simFULL$slope.SMR))+
          geom_line()+
          geom_point(size = 1)+
          scale_color_gradient(low = "black", high = "dodgerblue")+
          theme_light()+
          labs(x="Body mass (kg)", y=expression(MMR~mgO[`2`]*min^-1))+
          theme(legend.position = "none")
        p.mmrKG<-ggplot(data.simFULL, aes(BW_kg, pred.mmr.mgO2minKG,
                                          color = data.simFULL$slope.SMR,
                                          group = data.simFULL$slope.SMR))+
          geom_line()+
          geom_point(size = 1)+
          scale_color_gradient(low = "black", high = "dodgerblue")+
          theme_light()+
          labs(x="Body mass (kg)", y=expression(MMR~mgO[`2`]*min^-1*kg^-1))+
          theme(legend.position = "none")
        p.mmr.scale<-ggplot(data.simFULL, aes(x=log(BW_kg), y=log(pred.mmr.mgO2min),
                                              color = data.simFULL$slope.SMR,
                                              group = data.simFULL$slope.SMR ))+
          geom_smooth(method = "lm")+
          scale_color_gradient(low = "black", high = "dodgerblue")+
          theme_light()+
          labs(x="log(Body mass (kg))", y=expression(log(MMR~mgO[`2`]*min^-1)))+
          theme(legend.position = "none")
        p.smr<-ggplot(data.simFULL, aes(BW_kg, pred.smr.mgO2min,
                                        color = data.simFULL$slope.SMR,
                                        group = data.simFULL$slope.SMR))+
          geom_point(size = 1)+
          geom_line()+
          scale_color_gradient(low = "black", high = "red")+
          theme_light()+
          labs(x="Body mass (kg)", y=expression(SMR~mgO[`2`]*min^-1))+
          theme(legend.position = "none")
        p.smrKG<-ggplot(data.simFULL, aes(BW_kg, pred.smr.mgO2minKG,
                                          color = data.simFULL$slope.SMR,
                                          group = data.simFULL$slope.SMR))+
          geom_point(size = 1)+
          geom_line()+
          scale_color_gradient(low = "black", high = "red")+
          theme_light()+
          labs(x="Body mass (kg)", y=expression(SMR~mgO[`2`]*min^-1*kg^-1))+
          theme(legend.position = "none")
        p.smr.scale<-ggplot(data.simFULL, aes(x=log(BW_kg), y=log(pred.smr.mgO2min),
                                              color = data.simFULL$slope.SMR,
                                              group = data.simFULL$slope.SMR))+
          geom_smooth(method = "lm")+
          theme_light()+
          scale_color_gradient(low = "black", high = "red")+
          labs(x="log(Body mass (kg))", y=expression(log(SMR~mgO[`2`]*min^-1)))+
          theme(legend.position = "none")
        p.as<-ggplot(data.simFULL, aes(BW_kg, pred.as.mgO2min,
                                       color = data.simFULL$slope.SMR,
                                       group = data.simFULL$slope.SMR))+
          geom_point(size = 1)+
          geom_line()+
          scale_color_gradient(high = "#4FFBDF", low = "black")+
          theme_light()+
          labs(x="Body mass (kg)", y=expression(AS~mgO[`2`]*min^-1))+
          theme(legend.position = "none")
        p.asKG<-ggplot(data.simFULL, aes(BW_kg, pred.mmr.mgO2minKG - pred.smr.mgO2minKG,
                                         color = data.simFULL$slope.SMR,
                                         group = data.simFULL$slope.SMR))+
          geom_point(size = 1)+
          geom_line()+
          scale_color_gradient(high = "#4FFBDF", low = "black")+
          theme_light()+
          labs(x="Body mass (kg)", y=expression(AS~mgO[`2`]*min^-1*kg^-1))+
          theme(legend.position = "none")
        p.as.scale<-ggplot(data.simFULL, aes(x=log(BW_kg), y=log(pred.as.mgO2min),
                                             color = data.simFULL$slope.SMR,
                                             group = data.simFULL$slope.SMR))+
          geom_smooth(method = "lm")+
          theme_light()+
          scale_color_gradient(low = "black", high = "#4FFBDF")+
          labs(x="log(Body mass (kg))", y=expression(log(AS~mgO[`2`]*min^-1)))+
          theme(legend.position = "none")
        p.fas<-ggplot(data.simFULL, aes(BW_kg, pred.fas.mgO2min,
                                        color = data.simFULL$slope.SMR,
                                        group = data.simFULL$slope.SMR))+
          geom_point(size = 1)+
          geom_line()+
          scale_color_gradient(high = "#FF8066", low = "black")+
          theme_light() +
          labs(x="Body mass (kg)", y=expression(FAS))+
          theme(legend.position = "none")
        p.fas.scale<-ggplot(data.simFULL, aes(x=log(BW_kg), y=log(pred.fas.mgO2min),
                                              color = data.simFULL$slope.SMR,
                                              group = data.simFULL$slope.SMR))+
          geom_smooth(method = "lm")+
          theme_light()+
          scale_color_gradient(low = "black", high = "#FF8066")+
          labs(x="log(Body mass (kg))", y=expression(log(FAS)))+
          theme(legend.position = "none")
      }
      
      
      PLOT1<-plot_grid(title, 
                       plot_grid(p.mmr, p.mmrKG, p.mmr.scale, 
                                 p.smr, p.smrKG, p.smr.scale, 
                                 p.as, p.asKG, p.as.scale, 
                                 p.fas, NULL, p.fas.scale,
                                 nrow=4, align = "v",
                                 rel_heights = c(1, 0.8, 0.8, 0.8)),
                       ncol=1, rel_heights = c(0.1, 1))

      print(names(data.simFULL[!duplicated(data.simFULL),c(8, 10, 12, 14, 16)]))
      # wide to long 
      data.simFULL.p<-data.simFULL[!duplicated(data.simFULL),c("simID", "slope.SMR", "slope.MMR", "slope.AS", "slope.FAS")] %>%
        pivot_longer(cols = slope.SMR:slope.FAS,
                     names_to = "slopes",
                     values_to = "Vals") %>%
        as.data.frame()
      
      ii<-round(length(slopeseq.mmr)/2)
      
      data.simFULL.p$slopes<-as.factor(data.simFULL.p$slopes)
      data.simFULL.p$simID<-as.numeric(as.character(data.simFULL.p$simID))
      
      PLOT2<-ggplot(data.simFULL.p, aes(y = simID,
                                        fill = slopes,
                                        x = as.numeric(Vals),
                                        group = simID,
                                        label = Vals))+
        geom_point(pch=21, size=3)+
        scale_fill_manual(values = c("#4FFBDF", "#FF8066", "dodgerblue", "red"))+
        geom_hline(yintercept = c(1, ii, length(slopeseq.mmr)), color = "black", lty = 1)+
        # annotate(geom = "text", y = 25, x = 0.20, size= 3, hjust = 0, color = "black",
        #          label = expression(italic(b)[SMR]==italic(b)[MMR] ~ at ~ `0.75`))+
        geom_text(nudge_y = 0.1)+
        theme_light()+
        xlim(-0.5, 1.5)+
        labs(x = "Slope values", y = element_blank())+
        theme(legend.position = "top", 
              axis.text.y = element_blank(), 
              legend.title = element_blank())
      
      inset1<-ggplot(data.simFULL[data.simFULL$simID==1, ], aes(x=log(BW_kg), y=log(pred.as.mgO2min) ))+
        geom_smooth(method = "lm", color ="black" )+ # AS
        geom_smooth(aes(x=log(BW_kg), y=log(pred.mmr.mgO2min) ), method = "lm", color ="dodgerblue" )+ # MMR
        geom_smooth(aes(x=log(BW_kg), y=log(pred.smr.mgO2min) ), method = "lm", color ="red" )+ # SMR
        geom_smooth(aes(x=log(BW_kg), y=log(pred.fas.mgO2min) ), method = "lm", color ="#FF8066" )+ # FAS
        # lims(x = c(-4, 4), y = c(-4, 6))+
        geom_text(label = paste("bMMR =", data.simFULL.p[which(data.simFULL.p$simID==1 & data.simFULL.p$slopes == "slope.MMR")[1] , "Vals"]),
                  color ="dodgerblue", size=3, x = -0.25, y = -1.5, hjust = 0)+
        geom_text(label = paste("bSMR =", data.simFULL.p[which(data.simFULL.p$simID==1 & data.simFULL.p$slopes == "slope.SMR")[1] , "Vals"]),
                  color ="red", size=3, x = -0.25, y = -1.9, hjust = 0)+
        geom_text(label = paste("bAS =", data.simFULL.p[which(data.simFULL.p$simID==1 & data.simFULL.p$slopes == "slope.AS")[1] , "Vals"]),
                  color ="black", size=3, x = -0.25, y = -2.3, hjust = 0)+
        geom_text(label = paste("bFAS =", data.simFULL.p[which(data.simFULL.p$simID==1 & data.simFULL.p$slopes == "slope.FAS")[1] , "Vals"]),
                  color ="#FF8066", size=3, x = -0.25, y = -2.7, hjust = 0)+
        theme_light()+
        labs(x="log(Body mass (kg))", y=expression(log~(MR~mgO[`2`]*min^-1)))
      
      
      inset2<-ggplot(data.simFULL[data.simFULL$simID==ii, ], aes(x=log(BW_kg), y=log(pred.as.mgO2min) ))+
        geom_smooth(method = "lm", color ="black" )+ # AS
        geom_smooth(aes(x=log(BW_kg), y=log(pred.mmr.mgO2min) ), method = "lm", color ="dodgerblue" )+ # MMR
        geom_smooth(aes(x=log(BW_kg), y=log(pred.smr.mgO2min) ), method = "lm", color ="red" )+ # SMR
        geom_smooth(aes(x=log(BW_kg), y=log(pred.fas.mgO2min) ), method = "lm", color ="#FF8066" )+ # FAS
        # lims(x = c(-4, 4), y = c(-4, 6))+
        geom_text(label = paste("bMMR =", data.simFULL.p[which(data.simFULL.p$simID== ii & data.simFULL.p$slopes == "slope.MMR")[1] , "Vals"]),
                  color ="dodgerblue", size=3, x = -0.25, y = -1.5, hjust = 0)+
        geom_text(label = paste("bSMR =", data.simFULL.p[which(data.simFULL.p$simID==ii & data.simFULL.p$slopes == "slope.SMR")[1] , "Vals"]),
                  color ="red", size=3, x = -0.25, y = -1.9, hjust = 0)+
        geom_text(label = paste("bAS =", data.simFULL.p[which(data.simFULL.p$simID==ii & data.simFULL.p$slopes == "slope.AS")[1] , "Vals"]),
                  color ="black", size=3, x = -0.25, y = -2.3, hjust = 0)+
        geom_text(label = paste("bFAS =", data.simFULL.p[which(data.simFULL.p$simID==ii & data.simFULL.p$slopes == "slope.FAS")[1] , "Vals"]),
                  color ="#FF8066", size=3, x = -0.25, y = -2.7, hjust = 0)+
        theme_light()+
        labs(x="log(Body mass (kg))", y=expression(log~(MR~mgO[`2`]*min^-1)))
      
      inset3<-ggplot(data.simFULL[data.simFULL$simID==length(slopeseq.mmr), ], aes(x=log(BW_kg), y=log(pred.as.mgO2min) ))+
        geom_smooth(method = "lm", color ="black" )+ # AS
        geom_smooth(aes(x=log(BW_kg), y=log(pred.mmr.mgO2min) ), method = "lm", color ="dodgerblue" )+ # MMR
        geom_smooth(aes(x=log(BW_kg), y=log(pred.smr.mgO2min) ), method = "lm", color ="red" )+ # SMR
        geom_smooth(aes(x=log(BW_kg), y=log(pred.fas.mgO2min) ), method = "lm", color ="#FF8066" )+ # FAS
        # lims(x = c(-4, 4), y = c(-4, 6))+
        geom_text(label = paste("bMMR =", data.simFULL.p[which(data.simFULL.p$simID==length(slopeseq.mmr) & data.simFULL.p$slopes == "slope.MMR")[1] , "Vals"]),
                  color ="dodgerblue", size=3, x = -0.25, y = -1.5, hjust = 0)+
        geom_text(label = paste("bSMR =", data.simFULL.p[which(data.simFULL.p$simID==length(slopeseq.mmr) & data.simFULL.p$slopes == "slope.SMR")[1] , "Vals"]),
                  color ="red", size=3, x = -0.25, y = -1.9, hjust = 0)+
        geom_text(label = paste("bAS =", data.simFULL.p[which(data.simFULL.p$simID==length(slopeseq.mmr) & data.simFULL.p$slopes == "slope.AS")[1] , "Vals"]),
                  color ="black", size=3, x = -0.25, y = -2.3, hjust = 0)+
        geom_text(label = paste("bFAS =", data.simFULL.p[which(data.simFULL.p$simID==length(slopeseq.mmr) & data.simFULL.p$slopes == "slope.FAS")[1] , "Vals"]),
                  color ="#FF8066", size=3, x = -0.25, y = -2.7, hjust = 0)+
        theme_light()+
        labs(x="log(Body mass (kg))", y=expression(log~(MR~mgO[`2`]*min^-1)))
      
      
      inset1.massSpec<-ggplot(data.simFULL[data.simFULL$simID==1, ], aes(x=log(BW_kg), y=log(pred.as.mgO2min/BW_kg) ))+
        geom_smooth(method = "lm", color ="dodgerblue" )+ # MMR
        geom_smooth(aes(x=log(BW_kg), y=log(pred.as.mgO2minKG) ), method = "lm", color ="black" )+ # AS
        geom_smooth(aes(x=log(BW_kg), y=log(pred.smr.mgO2minKG) ), method = "lm", color ="red" )+ # SMR
        # lims(x = c(-4, 4), y = c(-4, 6))+
        theme_light()+
        labs(x="log(Body mass (kg))", y=expression(log~(MR~mgO[`2`]*min^-1*kg^-1)))
      
      
      inset2.massSpec<-ggplot(data.simFULL[data.simFULL$simID==ii, ], aes(x=log(BW_kg), y=log(pred.mmr.mgO2min/BW_kg) ))+
        geom_smooth(method = "lm", color ="dodgerblue" )+ # MMR
        geom_smooth(aes(x=log(BW_kg), y=log(pred.as.mgO2minKG) ), method = "lm", color ="black" )+ # AS
        geom_smooth(aes(x=log(BW_kg), y=log(pred.smr.mgO2minKG) ), method = "lm", color ="red" )+ # SMR
        # lims(x = c(-4, 4), y = c(-4, 6))+
        theme_light()+
        labs(x="log(Body mass (kg))", y=expression(log~(MR~mgO[`2`]*min^-1*kg^-1)))
      
      
      inset3.massSpec<-ggplot(data.simFULL[data.simFULL$simID==length(slopeseq.mmr), ], aes(x=log(BW_kg), y=log(pred.mmr.mgO2min/BW_kg) ))+
        geom_smooth(method = "lm", color ="dodgerblue" )+ # MMR
        geom_smooth(aes(x=log(BW_kg), y=log(pred.as.mgO2minKG) ), method = "lm", color ="black" )+ # AS
        geom_smooth(aes(x=log(BW_kg), y=log(pred.smr.mgO2minKG) ), method = "lm", color ="red" )+ # SMR
        # lims(x = c(-4, 4), y = c(-4, 6))+
        theme_light()+
        labs(x="log(Body mass (kg))", y=expression(log~(MR~mgO[`2`]*min^-1*kg^-1)))
      
      
      PLOT2.COMB<- plot_grid( PLOT2, 
                              plot_grid(inset3, inset2,  inset1, nrow=3), 
                              plot_grid(inset3.massSpec, inset2.massSpec,  inset1.massSpec, nrow=3), 
                              ncol = 3, rel_widths = c(1,0.6, 0.6) )
      
    }
  }
  
  return(list(data.simFULL, PLOT1, PLOT2.COMB))
}
# *******************************************

