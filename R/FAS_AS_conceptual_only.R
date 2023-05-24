

concept_FAS_AS<-function(
    data = data,
    colorMMR = "#00749F",
    colorRMR = "#C70039",
    colorAS = "#00C5A3",
    colorFAS = "#89A000", 
    colorShadeAS = "#C4FCEF", 
    colorShadeFAS = "#FEFEDF"){
  
    data <- data %>% 
      filter(data$pred.as.mgO2min > 0) %>% 
      as.data.frame() 
    data_bottom<-data[data$slope.SMR == 1, c("pred.smr.mgO2min", "BW_kg")]

    p1<-ggplot(data[data$simID==1,], aes(x=log(BW_kg), y=log(pred.mmr.mgO2min) ))+
      # geom_ribbon(mapping = aes(ymin = log(data_bottom$pred.smr.mgO2min)-log(2),
      #             ymax = log(pred.mmr.mgO2min)),
      #             alpha = 0.6,fill = colorShadeFAS)+
      # geom_ribbon(mapping = aes(ymin = log(data_bottom$pred.smr.mgO2min),
      #             ymax = log(pred.mmr.mgO2min)),
      #             alpha = 0.6,fill = colorShadeAS)+
      geom_abline(slope = data[data$simID==1,"slope.MMR"][1], intercept = data[data$simID==1,"int.MMR"][1], color = colorMMR, linewidth=1.5)+ # MMR
      geom_abline(slope = data[data$simID==1,"slope.SMR"][1], intercept = data[data$simID==1,"int.SMR"][1], color = colorRMR, linewidth=1.5)+ # MMR
      geom_abline(slope = data[data$simID==1,"slope.AS"][1], intercept = data[data$simID==1,"int.AS"][1], color = colorAS, linewidth=1.5)+ # MMR
      lims(x = c(-4, 4), y = c(-4, 5))+
      theme_light()+
      geom_text(mapping = aes(x = -2, y = 4, label = slope.FAS), color = colorFAS)+
      geom_text(mapping = aes(x = -2, y = 3.5, label = slope.AS), color = colorAS)+
      geom_text(mapping = aes(x = -2, y = 3, label = slope.MMR), color = colorMMR)+
      geom_text(mapping = aes(x = -2, y = 2.5, label = slope.SMR), color = colorRMR)+
      ggtitle(label = "", subtitle = "b(RMR) < b(MMR)  -->  b(FAS) > 0")
    ggformat2::ggformat(p1, y_title = expression(ln~MR~(mgO[`2`]*min^-1)), expression(ln~Mass~(kg)), size_text = 15)
    
    p2<-ggplot(data[data$simID==2,])+
      # geom_ribbon(mapping = aes(ymin = log(data_bottom$pred.smr.mgO2min)-log(2),
      #             ymax = log(pred.mmr.mgO2min)),
      #             alpha = 0.6,fill = colorShadeFAS)+
      # geom_ribbon(mapping = aes(ymin = log(pred.smr.mgO2min),
      #             ymax = log(pred.mmr.mgO2min)),
      #             alpha = 0.6,fill = colorShadeAS)+
      geom_abline(slope = data[data$simID==2,"slope.MMR"][1], intercept = data[data$simID==2,"int.MMR"][1], color = colorMMR, linewidth=1.5)+ # MMR
      geom_abline(slope = data[data$simID==2,"slope.SMR"][1], intercept = data[data$simID==2,"int.SMR"][1], color = colorRMR, linewidth=1.5)+ # MMR
      geom_abline(slope = data[data$simID==2,"slope.AS"][1], intercept = data[data$simID==2,"int.AS"][1], color = colorAS, linewidth=1.5)+ # MMR
      lims(x = c(-4, 4), y = c(-4, 5))+
      theme_light()+
      geom_text(mapping = aes(x = -2, y = 4, label = slope.FAS), color = colorFAS)+
      geom_text(mapping = aes(x = -2, y = 3.5, label = slope.AS), color = colorAS)+
      geom_text(mapping = aes(x = -2, y = 3, label = slope.MMR), color = colorMMR)+
      geom_text(mapping = aes(x = -2, y = 2.5, label = slope.SMR), color = colorRMR)+
      ggtitle(label = "", subtitle = "b(RMR) = b(MMR)  -->  b(FAS) = 0")
    ggformat2::ggformat(p2, y_title = expression(ln~MR~(mgO[`2`]*min^-1)), expression(ln~Mass~(kg)), size_text = 15)
    
    p3<-ggplot(data[data$simID==3,], aes(x=log(BW_kg), y=log(pred.mmr.mgO2min) ))+
      # geom_ribbon(mapping = aes(ymin = log(data_bottom$pred.smr.mgO2min)-log(2),
      #             ymax = log(pred.mmr.mgO2min)),
      #             alpha = 0.6,fill = colorShadeFAS)+
      # geom_ribbon(mapping = aes(ymin = log(pred.smr.mgO2min),
      #             ymax = log(pred.mmr.mgO2min)),
      #             alpha = 0.6,fill = colorShadeAS)+
      geom_abline(slope = data[data$simID==3,"slope.MMR"][1], intercept = data[data$simID==3,"int.MMR"][1], color = colorMMR, linewidth=1.5)+ # MMR
      geom_abline(slope = data[data$simID==3,"slope.SMR"][1], intercept = data[data$simID==3,"int.SMR"][1], color = colorRMR, linewidth=1.5)+ # MMR
      geom_abline(slope = data[data$simID==3,"slope.AS"][1], intercept = data[data$simID==3,"int.AS"][1], color = colorAS, linewidth=1.5)+ # MMR
      lims(x = c(-4, 4), y = c(-4, 5))+
      theme_light()+
      geom_text(mapping = aes(x = -2, y = 4, label = slope.FAS), color = colorFAS)+
      geom_text(mapping = aes(x = -2, y = 3.5, label = slope.AS), color = colorAS)+
      geom_text(mapping = aes(x = -2, y = 3, label = slope.MMR), color = colorMMR)+
      geom_text(mapping = aes(x = -2, y = 2.5, label = slope.SMR), color = colorRMR)+
      ggtitle(label = "", subtitle = "b(RMR) > b(MMR)  -->  b(FAS) < 0")

    ggformat2::ggformat(p3, y_title = expression(ln~MR~(mgO[`2`]*min^-1)), expression(ln~Mass~(kg)), size_text = 15)
    
    print(data[data$simID==3,])
    
    # # not log scale
    # p1.raw<-ggplot(data[data$simID==1,], aes(x=BW_kg, y=pred.mmr.mgO2min ))+
    #   geom_ribbon(mapping = aes(ymin = data_bottom$pred.smr.mgO2min-2,
    #               ymax = pred.mmr.mgO2min),
    #               alpha = 0.6,fill = colorShadeFAS)+
    #   geom_ribbon(mapping = aes(ymin = data_bottom$pred.smr.mgO2min,
    #               ymax = pred.mmr.mgO2min),
    #               alpha = 0.6,fill = colorShadeAS)+
    #   geom_smooth(method = "gam", formula = y ~ poly(x,2),
    #               color =colorMMR, se = FALSE)+ # MMR
    #   # geom_smooth(aes(x=BW_kg, y=pred.as.mgO2min ), method = "gam", color =colorAS , se = FALSE)+ # AS
    #   geom_smooth(aes(x=BW_kg, y=pred.smr.mgO2min),
    #               method = "gam", formula = y ~ poly(x,2),
    #               color =colorRMR, se = FALSE)+ # SMR
    #   geom_smooth(data = data_bottom, mapping = aes(x=BW_kg, y=pred.smr.mgO2min-2 ),
    #               method = "gam", formula = y ~ poly(x,2), 
    #               color = "black", se = FALSE )+ # SMR
    #   # lims(x = c(-3, 3), y = c(-3, 4))+
    #   theme_light()
    # ggformat2::ggformat(p1.raw, y_title = expression(MR~(mgO[`2`]*min^-1)), expression(Mass~(kg)), size_text = 15)
    # 
    # p2.raw<-ggplot(data[data$simID==2,], aes(x=BW_kg, y=pred.mmr.mgO2min ))+
    #   geom_ribbon(mapping = aes(ymin = data_bottom$pred.smr.mgO2min-2,
    #               ymax = pred.mmr.mgO2min),
    #               alpha = 0.6,fill = colorShadeFAS)+
    #   geom_ribbon(mapping = aes(ymin = pred.smr.mgO2min,
    #               ymax = pred.mmr.mgO2min),
    #               alpha = 0.6,fill = colorShadeAS)+
    #   geom_smooth(method = "gam", formula = y ~ poly(x,2),
    #               color =colorMMR, se = FALSE)+ # MMR
    #   # geom_smooth(aes(x=BW_kg, y=pred.as.mgO2min ), method = "gam", formula = y ~ poly(x,2),color =colorAS , se = FALSE)+ # AS
    #   geom_smooth(aes(x=BW_kg, y=pred.smr.mgO2min ),
    #               method = "gam", formula = y ~ poly(x,2),
    #               color =colorRMR, se = FALSE )+ # SMR
    #   geom_smooth(data = data_bottom, mapping = aes(x=BW_kg, y=pred.smr.mgO2min-2 ),
    #               method = "gam", formula = y ~ poly(x,2),
    #               color = "black", se = FALSE )+ # SMR
    #   # lims(x = c(-3, 3), y = c(-3, 4))+
    #   theme_light()
    # ggformat2::ggformat(p2.raw, y_title = expression(MR~(mgO[`2`]*min^-1)), expression(Mass~(kg)), size_text = 15)
    # 
    # p3.raw<-ggplot(data[data$simID==3,], aes(x=BW_kg, y=pred.mmr.mgO2min ))+
    #   geom_ribbon(mapping = aes(ymin = data_bottom$pred.smr.mgO2min-2,
    #               ymax = pred.mmr.mgO2min),
    #               alpha = 0.6,fill = colorShadeFAS)+
    #   geom_ribbon(mapping = aes(ymin = pred.smr.mgO2min,
    #               ymax = pred.mmr.mgO2min),
    #               alpha = 0.6,fill = colorShadeAS)+
    #   geom_smooth(method = "gam", formula = y ~ poly(x,2),
    #               color =colorMMR, se = FALSE)+ # MMR
    #   # geom_smooth(aes(x=BW_kg, y=pred.as.mgO2min ), method = "gam", formula = y ~ poly(x,2),color =colorAS , se = FALSE)+ # AS
    #   geom_smooth(aes(x=BW_kg, y=pred.smr.mgO2min ),
    #               method = "gam", formula = y ~ poly(x,2),
    #               color =colorRMR, se = FALSE )+ # SMR
    #   geom_smooth(data = data_bottom, mapping = aes(x=BW_kg, y=pred.smr.mgO2min-2),
    #               method = "gam", formula = y ~ poly(x,2),
    #               color = "black", se = FALSE )+ # SMR
    #   # lims(x = c(-3, 3), y = c(-3, 4))+
    #   theme_light()
    # ggformat2::ggformat(p3.raw, y_title = expression(MR~(mgO[`2`]*min^-1)), expression(Mass~(kg)), size_text = 15)
    # 
    
    return(list(p1, p2, p3))
    
}
