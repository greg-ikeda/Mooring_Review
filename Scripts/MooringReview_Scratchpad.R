test <- mooringdata_light %>%
  filter(`1_Depth_m` < 0)

unique(test$Datetime)


q <- ggplot(userdef)+
  geom_line(aes_string(x = variables[11],
                y = variables[8]),
            alpha = 0.3,
            size = 0.8)


q <- ggplot(userdef)+
  geom_line(aes_string(x = as.name("1_Chlorophyll_Fluorescence_ug/L"),
                y = as.name("1_Dissolved_Oxygen_mg/L")),
            alpha = 0.3,
            size = 0.8)

q
+
  geom_point(aes(x = `1_Chlorophyll_Fluorescence_ug/L`,
                 y = `1_Dissolved_Oxygen_mg/L`,
                 color = Datetime),
             size = 0.7)
ggplotly(q)


# Boxplots ----------------------------------------------------------------


boxplot_test_df <- mooringdata %>%
  filter(year == 2021,
         `Dissolved_Oxygen_%Sat` < 300) 

ggplot(boxplot_test_df)+
  geom_boxplot(fatten = 2,
               aes(x = as.factor(month),
                   y = `Dissolved_Oxygen_mg/L`))
