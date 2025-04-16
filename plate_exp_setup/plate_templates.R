### This script will create a set of templates that can be printed and used when setting up experiments in 6 or 12 well plates

library(tidyverse)

### Define Parameters

num_wells = 6 # 6 or 12 
num_plates = 10 # How many plates are being used? i.e. - how many templates should be made? 

treatment_levels = c("control", "1", "2") # Specify the treatments you want to distribute across plates

total_samples = num_wells * num_plates
print(total_samples)

templates = data.frame("plate_id" = rep(1:num_plates, each = num_wells), 
                       "column" = if(num_wells == 6){rep(c(1:3), times = 2)}else{rep(c(1:4), times = 3)},
                       "row" = if(num_wells == 6){rep(c(1:2), each = 3)}else{rep(c(1:3), each = 4)},
                       "sample" = rep(1:num_wells, times = num_plates),
                       "well_ids" = if(num_wells == 6){
                         paste(rep(c("A", "B"), each = num_wells/2), rep(c(1:3), times = 2), sep = "")
                       }else{
                         paste(rep(c("A", "B", "C"), each = num_wells/3), rep(c(1:4), times = 3), sep = "")
                       }) %>% 
  group_by(plate_id) %>% 
  mutate("treatment" = sample(x = rep(treatment_levels, 
                                      times = ceiling(num_wells / length(treatment_levels))), 
                              replace = F, 
                              size = num_wells))

coverage = templates %>% 
  group_by(plate_id) %>% 
  count(treatment) %>% 
  ungroup() %>% 
  complete(plate_id, treatment, fill = list(n = 0)) %>% 
  filter(n == 0)

if(dim(coverage)[1] > 0){
  print("ISSUE WITH MISSING TREATMENTS IN ONE OF THE PLATES")
}else{
  templates %>% ungroup() %>% count(treatment)
  
  write.csv(templates, "plate_setups.csv", row.names = F)
  
  for(plate in 1:num_plates){
    
    data = filter(templates, plate_id == plate)
    
    if(num_wells == 6){
      point_size = 48
      
      plate_template = ggplot(data, aes(x = column, y = row)) + 
        geom_point(shape = 1, size = point_size) + 
        geom_text(aes(label = treatment)) + 
        annotate(geom = "text", x = 0.55, y = 2.4, label = "A1") + 
        coord_cartesian(xlim = c(.52,max(data$column) + 0.48), ylim = c(0.51,max(data$row) + .49)) + 
        theme_void()
    }else{
        point_size = 25
        
        plate_template = ggplot(data, aes(x = column, y = row)) + 
          geom_point(shape = 1, size = point_size) + 
          geom_text(aes(label = treatment)) + 
          annotate(geom = "text", x = 0.62, y = 3.35, label = "A1") + 
          coord_cartesian(xlim = c(.25,max(data$column) + 0.75), ylim = c(0.5,max(data$row) + .5)) + 
          theme_void()
        }
    
    ggsave(plate_template, filename = paste0("plate",plate,".pdf"), path = "print_pages",
           width = 5.030, height = 3.365, units = "in")
  }
  
}

