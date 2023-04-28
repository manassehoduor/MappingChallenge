
#load libraries
pacman::p_load(ggtext,showtext,extrafont,tidyverse,janitor,hrbrthemes,viridis,sf,ggthemes,scales)

#@ Import fonts
font_add_google(name = "Barlow")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #MapChallenge <br>Source: 2019 Kenya Population & Housing Census (KPHC)")

# download Kenya county data
# load Kenya county data from GeoJSON file
kenya_counties <- st_read("C:/Users/ADMIN/Documents/R/#MapChallenge/kenyan-counties.geojson")

# religion affiliation data
religion <- read.csv(file = "religion_affiliation.csv", header = TRUE, sep = ",")

# clean religion data
religion <- religion |>
  mutate(across(2:15, ~as.numeric(str_replace_all(., ",", "")))) |>
  mutate_at(vars(2:15), as.numeric) |>
  drop_na()

# merge data
merged_df <- left_join(kenya_counties,religion, by = 'COUNTY')

# clean data merged data
merged_df_cln <- merged_df |>
  clean_names()

# plot
# catholic
ggplot(merged_df_cln) +
  geom_sf(aes(fill = catholic)) +
  theme_map() +
  scale_fill_viridis_c(option = "mako", direction = -1,
                       labels =  scales::label_number_si(), 
                       name = "Population") +
  theme(text = element_text(family = "Barlow"),
        plot.title = element_text(colour = "black",face = "bold",
                                      size = 15, hjust = 0.5, family = "Barlow"),
        plot.subtitle = element_text(colour = "black",size = 10,
                                         hjust = 0.5, family = "Barlow"),
        plot.caption = element_markdown(colour = 'black', hjust = 0, size = 7,
                                        family = 'Rosario', margin = margin(t = 5, b = 5)),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top",
        legend.box.background = element_blank(),
        legend.title.align = 0.5,
        legend.title = element_text(family = "Barlow", face = "bold", size = 10)) +
  labs(
    caption = cap,
    y = "",
    x = "",
    subtitle = "Most of Kenya's Catholic population, which comprises 20.6% of the total population\nis concentrated in Nairobi County",
    title = "\nCATHOLIC POPULATION") +
  guides(fill = guide_colourbar(direction = 'horizontal', ticks.linewidth=2,
                                barwidth = 10, barheight = 0.25, title.position = "top"),
           guide_legend(title.position = "top"))

ggsave("catholic.png", height=6, width=6, bg = "#89d942")

# Protestants
ggplot(merged_df_cln) +
  geom_sf(aes(fill = protestant)) +
  theme_map() +
  scale_fill_viridis_c(option = "mako", direction = -1,
                       labels =  scales::label_number_si(), 
                       name = "Population") +
  theme(text = element_text(family = "Barlow"),
        plot.title = element_text(colour = "black",face = "bold",
                                  size = 15, hjust = 0.5, family = "Barlow"),
        plot.subtitle = element_text(colour = "black",size = 10,
                                     hjust = 0.5, family = "Barlow"),
        plot.caption = element_markdown(colour = 'black', hjust = 0, size = 7,
                                        family = 'Rosario', margin = margin(t = 5, b = 5)),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top",
        legend.box.background = element_blank(),
        legend.title.align = 0.5,
        legend.title = element_text(family = "Barlow", face = "bold", size = 10)) +
  labs(
    caption = cap,
    y = "",
    x = "",
    subtitle = "Most of Kenya's Protestant population, which comprises 33.4% of the total population\nis concentrated in Nairobi County",
    title = "\nPROTESTANT POPULATION") +
  guides(fill = guide_colourbar(direction = 'horizontal', ticks.linewidth=2,
                                barwidth = 10, barheight = 0.25, title.position = "top"),
         guide_legend(title.position = "top"))

ggsave("protestant.png", height=6, width=6, bg = "#89d942")


# Evangelical churches
ggplot(merged_df_cln) +
  geom_sf(aes(fill = evangelical_churches)) +
  theme_map() +
  scale_fill_viridis_c(option = "mako", direction = -1,
                       labels =  scales::label_number_si(), 
                       name = "Population") +
  theme(text = element_text(family = "Barlow"),
        plot.title = element_text(colour = "black",face = "bold",
                                  size = 15, hjust = 0.5, family = "Barlow"),
        plot.subtitle = element_text(colour = "black",size = 10,
                                     hjust = 0.5, family = "Barlow"),
        plot.caption = element_markdown(colour = 'black', hjust = 0, size = 7,
                                        family = 'Rosario', margin = margin(t = 5, b = 5)),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top",
        legend.box.background = element_blank(),
        legend.title.align = 0.5,
        legend.title = element_text(family = "Barlow", face = "bold", size = 10)) +
  labs(
    caption = cap,
    y = "",
    x = "",
    subtitle = "Most of Kenya's Evangelic population, which comprises 20.4% of the total population\nis concentrated in Nairobi County",
    title = "\nEVANGELIC POPULATION") +
  guides(fill = guide_colourbar(direction = 'horizontal', ticks.linewidth=2,
                                barwidth = 10, barheight = 0.25, title.position = "top"),
         guide_legend(title.position = "top"))

ggsave("evangelical.png", height=6, width=6, bg = "#89d942")


# Islam
ggplot(merged_df_cln) +
  geom_sf(aes(fill = islam)) +
  theme_map() +
  scale_fill_viridis_c(option = "mako", direction = -1,
                       labels =  scales::label_number_si(), 
                       name = "Population") +
  theme(text = element_text(family = "Barlow"),
        plot.title = element_text(colour = "black",face = "bold",
                                  size = 15, hjust = 0.5, family = "Barlow"),
        plot.subtitle = element_text(colour = "black",size = 10,
                                     hjust = 0.5, family = "Barlow"),
        plot.caption = element_markdown(colour = 'black', hjust = 0, size = 7,
                                        family = 'Rosario', margin = margin(t = 5, b = 5)),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top",
        legend.box.background = element_blank(),
        legend.title.align = 0.5,
        legend.title = element_text(family = "Barlow", face = "bold", size = 10)) +
  labs(
    caption = cap,
    y = "",
    x = "",
    subtitle = "The Eastern parts of Kenya have a concentration of the Islam population,\n which accounts for 10.9% of the total population in Kenya",
    title = "\nMUSLIM POPULATION") +
  guides(fill = guide_colourbar(direction = 'horizontal', ticks.linewidth=2,
                                barwidth = 10, barheight = 0.25, title.position = "top"),
         guide_legend(title.position = "top"))

ggsave("Islam.png", height=6, width=6, bg = "#89d942")



