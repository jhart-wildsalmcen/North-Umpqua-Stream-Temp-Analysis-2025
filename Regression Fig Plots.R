## Air/Water Regression Plot

# Air logger
air <- air_logger("21679162") 

# Water logger
water <- water_logger("21679180")

# Join air/water readings
airwater_join <- logger_join()

# Calculate Pearson correlation coefficient
correlation <- cor(airwater_join$water_max, airwater_join$air_mean, method = 'pearson')
print(paste("Pearson R:",correlation))

# Linear model fit summary
fit <- lm(water_max ~ air_mean, data = airwater_join)
summary(fit)

# Create regression plot
reg_plot_21679180 <- ggplot(airwater_join, aes(x= air_mean, y = water_max)) +
  geom_point(shape= 21, color='black', fill= 'deepskyblue3', size = 3, stroke = 1) +
  geom_smooth(method = "lm", se = TRUE, colour = 'black') +
  theme_light() + 
  labs(title="Steamboat above Big Bend Cr. (Air - N. Umpqua at Steamboat)",
       # subtitle = "Max Water Temp ~ Mean Air Temp",
       x="Daily Mean Air temperature (\u00B0C)",
       y="Daily Max Water temperature (\u00B0C)") + 
  geom_text(x = 16, y = 24, label = lm_eqn(airwater_join), parse = TRUE, size = 5)  + 
  geom_text(x = 16, y = 23, label = paste("Pearson R:", round(correlation,3)), size = 5) +
  theme(plot.subtitle = element_text(size = 11),
        axis.text=element_text(size= 12),
        axis.title=element_text(size= 14),
        plot.title = element_textbox(
          size = 16,
          color = "white", fill = "#5D729D", box.color = "#4A618C",
          halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
          padding = margin(2, 1, 1, 1), margin = margin(3, 3, 3, 3)
        )) 

reg_plot_21679180

# Export plot to file
 ggsave(reg_plot_21679180, file="Figure Plots/Regression Plots/Figure Size/regression_plot_SteamboatAboveBigBend.png", height = 4, width = 8)

 
 
 
 air <- air_logger("21679162") 
 
 # Water logger
 water <- BigBend %>% 
   filter(longdate > '2024-06-19' & longdate <= '2024-09-15') %>% 
   mutate(water_max = max_temp)
 
 # Join air/water readings
 airwater_join <- logger_join()
 
 # Calculate Pearson correlation coefficient
 correlation <- cor(airwater_join$water_max, airwater_join$air_mean, method = 'pearson')
 print(paste("Pearson R:",correlation))
 
 # Linear model fit summary
 fit <- lm(water_max ~ air_mean, data = airwater_join)
 summary(fit)
 
 # Create regression plot
 reg_plot_15170014 <- ggplot(airwater_join, aes(x=air_mean, y = water_max)) +
   geom_point(shape= 21, color='black', fill= 'deepskyblue3', size = 3, stroke=1) +
   geom_smooth(method = "lm", se = TRUE, colour = 'black') +
   theme_light() + 
   labs(title="Big Bend Cr. at mouth (Air - N. Umpqua at Steamboat)",
        #subtitle = "Max Water Temp ~ Mean Air Temp",
        x="Daily Mean Air temperature (\u00B0C)",
        y="Daily Max Water temperature (\u00B0C)") + 
   geom_text(x = 16, y = 19, label = lm_eqn(airwater_join), parse = TRUE, size = 5)  + 
   geom_text(x = 16, y = 18, label = paste("Pearson R:", round(correlation,3)), size =5) +
   theme(plot.subtitle = element_text(size = 11),
         axis.text=element_text(size= 12),
         axis.title=element_text(size= 14),
         plot.title = element_textbox(
           size = 16,
           color = "white", fill = "#5D729D", box.color = "#4A618C",
           halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
           padding = margin(2, 1, 1, 1), margin = margin(3, 3, 3, 3)
         )) 
 
 reg_plot_15170014
 
 # Export plot to file
  ggsave(reg_plot_15170014, file="Figure Plots/Regression Plots/Figure Size/regression_plot_BigBend.png", height = 4, width = 8)
  
  
  
  
  ## Air/Water Regression Plot
  
  # Air logger
  air <- air_logger("21679162") 
  
  # Water logger
  water <- water_logger("21679175")
  
  # Join air/water readings
  airwater_join <- logger_join()
  
  # Calculate Pearson correlation coefficient
  correlation <- cor(airwater_join$water_max, airwater_join$air_mean, method = 'pearson')
  print(paste("Pearson R:",correlation))
  
  fit <- lm(water_max ~ air_mean, data = airwater_join)
  summary(fit)
  
  # Create regression plot
  
  reg_plot_21679175 <- ggplot(airwater_join, aes(x=air_mean, y = water_max)) +
    geom_point(shape= 21, color='black', fill= 'deepskyblue3', size = 3, stroke=1) +
    geom_smooth(method = "lm", se = TRUE, colour = 'black') +
    theme_light() + 
    labs(title="Steamboat below Big Bend Cr. (Air - N. Umpqua at Steamboat)",
         #subtitle = "Max Water Temp ~ Mean Air Temp",
         x="Daily Mean Air temperature (\u00B0C)",
         y="Daily Max Water temperature (\u00B0C)") + 
    geom_text(x = 16, y = 21, label = lm_eqn(airwater_join), parse = TRUE, size =5)  + 
    geom_text(x = 16, y = 20, label = paste("Pearson R:", round(correlation,3)), size =5) +
    theme(plot.subtitle = element_text(size = 11),
          axis.text=element_text(size= 12),
          axis.title=element_text(size= 14),
          plot.title = element_textbox(
            size = 16,
            color = "white", fill = "#5D729D", box.color = "#4A618C",
            halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
            padding = margin(2, 1, 1, 1), margin = margin(3, 3, 3, 3)
          )) 
  
  reg_plot_21679175
  
  # Export plot to file
   ggsave(reg_plot_21679175, file="Figure Plots/Regression Plots/Figure Size/regression_plot_SteamboatBelowBigBend.png", height = 4, width = 8)
  
   
   
   
   ## Air/Water Regression Plot
   
   # Air logger
   air <- air_logger("21679162")
   
   # Water logger
   water <- Steamboat_AboveCant  %>% 
     filter(longdate > '2024-06-19' & longdate <= '2024-09-15') %>% 
     mutate(water_max = max_temp)
   
   # Join air/water readings
   airwater_join <- logger_join()
   
   # Calculate Pearson correlation coefficient
   correlation <- cor(airwater_join$water_max, airwater_join$air_mean, method = 'pearson')
   print(paste("Pearson R:",correlation))
   
   fit <- lm(water_max ~ air_mean, data = airwater_join)
   summary(fit)
   
   
   # Create regression plot
   reg_plot_15170003 <- ggplot(airwater_join, aes(x=air_mean, y = water_max)) +
     geom_point(shape= 21, color='black', fill= 'deepskyblue3', size = 3, stroke=1) +
     geom_smooth(method = "lm", se = TRUE, colour = 'black') +
     theme_light() + 
     labs(title="Steamboat above Canton Cr. (Air - N. Umpqua at Steamboat)",
          #subtitle = "Max Water Temp ~ Mean Air Temp",
          x="Daily Mean Air temperature (\u00B0C)",
          y="Daily Max Water temperature (\u00B0C)") + 
     geom_text(x = 16, y = 24, label = lm_eqn(airwater_join), parse = TRUE, size=5)  + 
     geom_text(x = 16, y = 23, label = paste("Pearson R:", round(correlation,3)), size=5) +
     theme(plot.subtitle = element_text(size = 11),
           axis.text=element_text(size= 12),
           axis.title=element_text(size= 14),
           plot.title = element_textbox(
             size = 16,
             color = "white", fill = "#5D729D", box.color = "#4A618C",
             halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
             padding = margin(2, 1, 1, 1), margin = margin(3, 3, 3, 3)
           )) 
   
   reg_plot_15170003
   
   # Export plot to file
    ggsave(reg_plot_15170003, file="Figure Plots/Regression Plots/Figure Size/regression_plot_SteamboatAboveCanton.png", height = 4, width = 8)
    
    
    
    air <- air_logger("21679162")
    
    # Water logger
    water <- water_logger("21679177")
    
    # Join air/water readings
    airwater_join <- logger_join()
    
    # Calculate Pearson correlation coefficient
    correlation <- cor(airwater_join$water_max, airwater_join$air_mean, method = 'pearson')
    print(paste("Pearson R:",correlation))
    
    # Summary of linear model fit
    fit <- lm(water_max ~ air_mean, data = airwater_join)
    summary(fit)
    
    
    # Create regression plot
    reg_plot_21679177 <- ggplot(airwater_join, aes(x=air_mean, y = water_max)) +
      geom_point(shape= 21, color='black', fill= 'deepskyblue3', size = 3, stroke=1) +
      geom_smooth(method = "lm", se = TRUE, colour = 'black') +
      theme_light() + 
      labs(title="Upper Canton Cr. (Air - N. Umpqua at Steamboat)",
           #subtitle = "Max Water Temp ~ Mean Air Temp",
           x="Daily Mean Air temperature (\u00B0C)",
           y="Daily Max Water temperature (\u00B0C)") + 
      geom_text(x = 16, y = 21, label = lm_eqn(airwater_join), parse = TRUE, size=5)  + 
      geom_text(x = 16, y = 20.25, label = paste("Pearson R:", round(correlation,3)),size=5) +
      theme(plot.subtitle = element_text(size = 11),
            axis.text=element_text(size= 12),
            axis.title=element_text(size= 14),
            plot.title = element_textbox(
              size = 16,
              color = "white", fill = "#5D729D", box.color = "#4A618C",
              halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
              padding = margin(2, 1, 1, 1), margin = margin(3, 3, 3, 3)
            )) 
    
    
    reg_plot_21679177
    
    # Export plot to file
     ggsave(reg_plot_21679177, file="Figure Plots/Regression Plots/Figure Size/regression_plot_UpperCanton.png", height = 4, width = 8)
     
     
     
     air <- air_logger("21679162")
     
     # Water logger
     water <- water_logger("21679176")
     
     # Join air/water readings
     airwater_join <- logger_join()
     
     # Calculate Pearson correlation coefficient
     correlation <- cor(airwater_join$water_max, airwater_join$air_mean, method = 'pearson')
     print(paste("Pearson R:",correlation))
     
     # Summary of linear model fit
     fit <- lm(water_max ~ air_mean, data = airwater_join)
     summary(fit)
     
     
     # Create regression plot
     reg_plot_21679176 <- ggplot(airwater_join, aes(x=air_mean, y = water_max)) +
       geom_point(shape= 21, color='black', fill= 'deepskyblue3', size = 3, stroke=1) +
       geom_smooth(method = "lm", se = TRUE, colour = 'black') +
       theme_light() + 
       labs(title="Lower Canton Cr. (Air - N. Umpqua at Steamboat)",
            #subtitle = "Max Water Temp ~ Mean Air Temp",
            x="Daily Mean Air temperature (\u00B0C)",
            y="Daily Max Water temperature (\u00B0C)") + 
       geom_text(x = 16, y = 21, label = lm_eqn(airwater_join), parse = TRUE, size=5)  + 
       geom_text(x = 16, y = 20.25, label = paste("Pearson R:", round(correlation,3)),size=5) +
       theme(plot.subtitle = element_text(size = 11),
             axis.text=element_text(size= 12),
             axis.title=element_text(size= 14),
             plot.title = element_textbox(
               size = 16,
               color = "white", fill = "#5D729D", box.color = "#4A618C",
               halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
               padding = margin(2, 1, 1, 1), margin = margin(3, 3, 3, 3)
             )) 
     
     
     reg_plot_21679176
     
     # Export plot to file
     ggsave(reg_plot_21679176, file="Figure Plots/Regression Plots/Figure Size/regression_plot_LowerCanton.png", height = 4, width = 8)
     
     
     
     
     
     
     
     
     
     
     
     
     air <- air_logger("21679162")
     
     # Water logger
     water <- water_logger("21433141")
     
     # Join air/water readings
     airwater_join <- logger_join()
     
     # Calculate Pearson correlation coefficient
     correlation <- cor(airwater_join$water_max, airwater_join$air_mean, method = 'pearson')
     print(paste("Pearson R:",correlation))
     
     # Summary linear model fit
     fit <- lm(water_max ~ air_mean, data = airwater_join)
     summary(fit)
     
     
     # Create regression plot
     reg_plot_21433141 <- ggplot(airwater_join, aes(x=air_mean, y = water_max)) +
       geom_point(shape= 21, color='black', fill= 'deepskyblue3', size = 3,stroke=1) +
       geom_smooth(method = "lm", se = TRUE, colour = 'black') +
       theme_light() + 
       labs(title="North Umpqua above Steamboat Cr. (Air - N. Umpqua at Steamboat)",
            #subtitle = "Max Water Temp ~ Mean Air Temp",
            x="Daily Mean Air temperature (\u00B0C)",
            y="Daily Max Water temperature (\u00B0C)") + 
       geom_text(x = 16, y = 19, label = lm_eqn(airwater_join), parse = TRUE, size=5)  + 
       geom_text(x = 16, y = 18.2, label = paste("Pearson R:", round(correlation,3)), size=5) +
       theme(plot.subtitle = element_text(size = 11),
             axis.text=element_text(size= 12),
             axis.title=element_text(size= 14),
             plot.title = element_textbox(
               size = 16,
               color = "white", fill = "#5D729D", box.color = "#4A618C",
               halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
               padding = margin(2, 1, 1, 1), margin = margin(3, 3, 3, 3)
             )) 
     
     reg_plot_21433141
     
     #Export plot to file
     ggsave(reg_plot_21433141, file="Figure Plots/Regression Plots/Figure Size/regression_plot_NUmpqua_AboveSteam.png", height = 4, width = 8)
     
     
     
     air <- air_logger("21433138")
     
     # Water logger
     water <- FishCr %>% 
       filter(longdate > '2024-06-19' & longdate <= '2024-09-15') %>% 
       mutate(water_max = max_temp)
     
     # Join air/water readings
     airwater_join <- logger_join()
     
     # Calculate Pearson correlation coefficient
     correlation <- cor(airwater_join$water_max, airwater_join$air_mean, method = 'pearson')
     print(paste("Pearson R:",correlation))
     
     # Summary linear model fit
     fit <- lm(water_max ~ air_mean, data = airwater_join)
     summary(fit)
     
     
     # Create regression plot
     reg_plot_15110003 <- ggplot(airwater_join, aes(x=air_mean, y = water_max)) +
       geom_point(shape= 21, color='black', fill= 'deepskyblue3', size = 3,stroke=1) +
       geom_smooth(method = "lm", se = TRUE, colour = 'black') +
       theme_light() + 
       labs(title="Fish Cr. at mouth (Air - Copeland Cr. at mouth)",
            #subtitle = "Max Water Temp ~ Mean Air Temp",
            x="Daily Mean Air temperature (\u00B0C)",
            y="Daily Max Water temperature (\u00B0C)") + 
       geom_text(x = 15, y = 17.5, label = lm_eqn(airwater_join), parse = TRUE,size=5)  + 
       geom_text(x = 15, y = 16.8, label = paste("Pearson R:", round(correlation,3)),size=5) +
       theme(plot.subtitle = element_text(size = 11),
             axis.text=element_text(size= 12),
             axis.title=element_text(size= 14),
             plot.title = element_textbox(
               size = 16,
               color = "white", fill = "#5D729D", box.color = "#4A618C",
               halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
               padding = margin(2, 1, 1, 1), margin = margin(3, 3, 3, 3)
             )) 
     
     reg_plot_15110003
     
     #Export plot to file
     ggsave(reg_plot_15110003, file="Figure Plots/Regression Plots/Figure Size/regression_plot_FishCreek.png", height = 4, width = 8)
     
  