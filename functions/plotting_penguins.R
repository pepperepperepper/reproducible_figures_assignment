# Function to plot culmen length against flipper length
plot_culmen_flipper_figure <- function(culmen_flipper, x, y){
  ggplot(culmen_flipper, aes(x = culmen_length_mm, y = flipper_length_mm)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 2,
             alpha = 0.8) +
  scale_color_manual(values = c("cornflowerblue","darkolivegreen2","darkorange")) +
  labs(title = "Scatterplot showing culmen length against flipper length",
       subtitle = "For Adelie, Chinstrap and Gentoo Penguins",
       x = "Culmen length (mm)",
       y = "Flipper length (mm)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme_bw()
}

# Function to save the culmen_flipper figure as a .png file
save_culmen_flipper_figure_png <- function(culmen_flipper, filename, size, res, scaling){
  agg_png(filename, 
          width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  culmen_flipper_figure <- plot_culmen_flipper_figure(culmen_flipper)
  print(culmen_flipper_figure)
  dev.off()
}

# Function to save the culmen_flipper figure as a .svg file
save_culmen_flipper_figure_svg <- function(culmen_flipper, filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width = size_inches, height = size_inches, scaling = scaling)
  culmen_flipper_figure <- plot_culmen_flipper_figure(culmen_flipper)
  print(culmen_flipper_figure)
  dev.off()
}

# Function to plot chinstrap figure
plot_chinstrap_figure <- function(penguin_data, x, y){
  ggplot(penguin_data, aes(x = culmen_length_mm, y = flipper_length_mm)) +
    geom_point(color = "cornflowerblue",
               size = 2,
               alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    labs(title = "Scatterplot with a linear regression line showing culmen against flipper length",
         subtitle = "For Chinstrap penguins on Dream island",
         x = "Culmen length (mm)",
         y = "Flipper length (mm)",) +
    theme_bw()
}

# Function to save the chinstrap_only figure as a .png file
save_chinstrap_figure_png <- function(penguins_data, filename, size, res, scaling){
  agg_png(filename, 
          width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  chinstrap_figure <- plot_chinstrap_figure(penguins_data)
  print(chinstrap_figure)
  dev.off()
}

# Function to save the chinstrap_only figure as a .svg file
save_chinstrap_figure_svg <- function(penguins_data, filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width = size_inches, height = size_inches, scaling = scaling)
  chinstrap_figure <- plot_chinstrap_figure(penguins_data)
  print(chinstrap_figure)
  dev.off()
}

# Function to plot chinstrap figure with sex
plot_chinstrap_sexes_figure <- function(chinstrap_only_sexes, x, y){
  ggplot(chinstrap_only_sexes, aes(x = culmen_length_mm, y = flipper_length_mm, color = sex, shape = sex)) +
    geom_point(size = 2,
               alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    scale_color_manual(values = c("cornflowerblue", "darkorange")) +
    labs(title = "Scatterplot with a linear regression line showing culmen against flipper length",
         subtitle = "For male and female Chinstrap penguins on Dream island",
         x = "Culmen length (mm)",
         y = "Flipper length (mm)",
         color = "Penguin sex",
         shape = "Penguin sex") +
    theme_bw()
}
