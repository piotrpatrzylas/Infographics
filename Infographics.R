#@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@     INFOGRAPHICS v.02    @#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
##############################
#   INPUT INFORMATION        #
##############################

# Title - enter number from the list below
info_type <- 2
# 1 - C. difficile infection
# 2 - E. coli bacteraemia
# 3 - Klebsiella spp. bacteraemia
# 4 - MRSA bacteraemia
# 5 - MSSA bacteraemia
# 6 - Pseudomonas Aeruginosa Bacteraemia

# Overall rate (top)
rate_number <- 9.8    #must be numeric
plot_data   <- data.frame(years = c("2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2017/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24", "2024/25", "2025/26", "2026/27", "2027/28"),
                        rate = c(15, 15.5, 15.7, 20, 33, 15, 15.5, 15.7, 20, 33, 15, 24, 24, 65, 31))

# Risk greater among (middle) - enter pair of numbers from the list below
rhigh_factor  <- 7    #Max risk age group 
rlow_factor   <- 4    #Min risk age grop
# 1 - infants         (age < 1)
# 2 - youth           (age  1 - 14)
# 3 - young adults    (age 15 - 44)
# 4 - adults          (age 45 - 64)
# 5 - older adults    (age 65 - 74)
# 6 - senior adults   (age 75 - 84)
# 7 - elderly         (>= 85)

rhigh_malenumber  <- 185  #must be numeric
rhigh_femnumber   <- 88   #must be numeric
rlow_malenumber   <- 29   #must be numeric
rlow_femnumber    <- 13   #must be numeric

#Most common source of infection (bottom left) - may require formatting with \n for new line
doughnut_data     <- data.frame(
  count = c(22, 23, 14, 6, 25, 10),
  source = c("Unknown", "Other\nsources", "Respiratory\ntract", "Gastrointestinal", "Hepatibiliary", "UTI"))

#Most cases (bottom right)
community_percent <- 66   #must be numeric
hospital_percent <- 33    #must be numeric

#CDI Only
previous_year <- "2007/08"

community_percent_old <- 40
hospital_percent_old <- 60
community_percent_new <- 60
hospital_percent_new <- 40

# Change manually location / year in the title:
location_manually <- NULL
year_manually <- NULL

##############################
#       LIBRARIES            #
##############################

library(useful)           # printing images to pdf
library(grid)             # pdf layout
library(magick)           # image processing
library(ggplot2)          # basic plots
library(cowplot)          # complex plots
library(stringr)          # string operations 
library(RColorBrewer)     # colour palette for bottom left figure
library(Cairo)            # unicode characters in pdf
library(gridExtra)        # waffle chart
library(ggrepel)          # text align in plots

##############################
#       PRE-PROCESSING       #
#       &                    #
#       COLOURS              #
##############################
# general
this_year <- substr(as.character(Sys.Date()), 1, 4)

#title
if (info_type == 1) {
  title_name1 <- "C. difficile"
  italize_title <- T
  title_name2 <- "Infection"
  plot_name <- bquote("Trands in rates of"~italic(.("C. difficile")) ~"bacteraemia")
  infographics_colour   <- "#ff8c00"
  infographics_colour2  <- "#ffa500"
} else if (info_type == 2) {
  title_name1 <- "E. coli"
  italize_title <- T
  title_name2 <- "Bacteraemia"
  plot_name <- bquote("Trands in rates of"~italic(.("E. coli")) ~"bacteraemia")
  infographics_colour   <- "#00008b"
  infographics_colour2  <- "#72bcd4"
} else if (info_type == 3) {
  title_name1 <- "Klebsiella spp."
  italize_title <- T
  title_name2 <- "Bacteraemia"
  plot_name <- bquote("Trands in rates of"~italic(.("Klebsiella spp.")) ~"bacteraemia")
  infographics_colour   <- "#008000"
  infographics_colour2  <- "#8fbc8f"
} else if (info_type == 4) {
  title_name1 <- "MRSA"
  italize_title <- F
  title_name2 <- "Bacteraemia"
  plot_name <- bquote("Trands in rates of MRSA bacteraemia")
  infographics_colour   <- "#cd5c5c"
  infographics_colour2  <- "#db7093"
} else if (info_type == 5) {
  title_name1 <- "MSSA"
  italize_title <- F
  title_name2 <- "Bacteraemia"
  plot_name <- bquote("Trands in rates of MSSA bacteraemia")
  infographics_colour   <- "#00468b"
  infographics_colour2  <- "#add8e6"
} else if (info_type == 6) {
  title_name1 <- "Pseudomonas Aeruginosa"
  italize_title <- T
  title_name2 <- "Bactereamia"
  plot_name <- bquote("Trands in rates of"~italic(.("Pseudomonas Aeruginosa")))
  infographics_colour   <- "#ffd700"
  infographics_colour2  <- "#cccc00"
}

get_waffle_integers <- floor(rate_number)
waffle_decimals <- rate_number - get_waffle_integers

if (is.null(location_manually)) {
  title_place <- "England"
} else {title_place = location_manually}

if (is.null(year_manually)) {
  title_year <- paste0(as.numeric(this_year)-1, "/", this_year)
} else {title_year = paste0(as.numeric(year_manually) -1, "/", year_manually)}
  
#middle
switch(rhigh_factor, 
       "infant" = {
         rhigh_factor   <- "infants"
         rhigh_factor2  <- "infant"
         rhigh_age <- paste("(age < 1)")
       },
       "youth" = {
         rhigh_factor   <- "youth"
         rhigh_factor2  <- "youth"
         rhigh_age <- paste("(age 1 - 14)")
       },
       "young_adult" = {
         rhigh_factor   <- "adults"
         rhigh_factor2  <- "adult"
         rhigh_age <- paste("(age 15 - 44)")
       },
       "adult" = {
         rhigh_factor   <- "adults"
         rhigh_factor2  <- "adult"
         rhigh_age <- paste("(age 45 - 64)")
       },
       "older_adult" = {
         rhigh_factor   <- "adults"
         rhigh_factor2  <- "adult"
         rhigh_age <- paste("(age 65 - 74)")
       },
       "senior_adult" = {
         rhigh_factor   <- "adults"
         rhigh_factor2  <- "adult"
         rhigh_age <- paste("(age 75 - 84)")
       },
       "elder" = {
         rhigh_factor   <- "elderly"
         rhigh_factor2  <- "elderly" 
         rhigh_age <- paste("(age \U2265 85)")
       })

switch(rlow_factor, 
       "infant" = {
         rlow_factor  <- "infants"
         rlow_factor2 <- "infant"
         rlow_age <- paste("(age < 1)")
       },
       "youth" = {
         rlow_factor  <- "youth"
         rlow_factor2 <- "youth"
         rlow_age <- paste("(age 1 - 14)")
       },
       "young_adult" = {
         rlow_factor  <- "adults"
         rlow_factor2 <- "adult"
         rlow_age <- paste("(age 15 - 44)")
       },
       "adult" = {
         rlow_factor  <- "adults"
         rlow_factor2 <- "adult"
         rlow_age <- paste("(age 45 - 64)")
       },
       "older_adult" = {
         rlow_factor  <- "adults"
         rlow_factor2 <- "adult"
         rlow_age <- paste("(age 65 - 74)")
       },
       "senior_adult" = {
         rlow_factor  <- "adults"
         rlow_factor2 <- "adult"
         rlow_age <- paste("(age 75 - 84)")
       },
       "elder" = {
         rlow_factor  <- "elderly"
         rlow_factor2 <- "elderly"
         rlow_age <- paste("(age \U2265 85)")
       })

#bottom left
common_filter <- c("Unknown", "Other")
common_data <- subset(doughnut_data, !source %in% common_filter)
common_source <- str_replace(as.character(common_data[which.max(common_data$count), 2]), pattern = "\n", replacement = " ")

#bottom right
if (community_percent < hospital_percent) {
  most_cases <- "hospital-onset"
} else if (hospital_percent < community_percent) {
  most_cases <- "community-onset"
} else if (hospital_percent == community_percent) {
  most_cases <- NULL
}


#CDI ONLY
if (community_percent_old < hospital_percent_old) {
  most_cases_old <- "hospital-onset"
} else if (hospital_percent_old < community_percent_old) {
  most_cases_old <- "community-onset"
} else if (hospital_percent_old == community_percent_old) {
  most_cases_old <- NULL
}

if (community_percent_new < hospital_percent_new) {
  most_cases_new <- "hospital-onset"
} else if (hospital_percent_new < community_percent_new) {
  most_cases_new <- "community-onset"
} else if (hospital_percent_new == community_percent_new) {
  most_cases_new <- NULL
}


#colours
background_colour     <- "#efe3af"

palette_fun = colorRampPalette(c(infographics_colour, infographics_colour2))
doughnut_palette <- palette_fun(dim(doughnut_data)[1])

##############################
#       EXTERNAL FILES       #
##############################

logo_location <- "logo.jpg"
left_squarecircle <- "left square circle.jpg"
right_squarecircle <- "right square circle.jpg"
left_rectcircle <- "left rect cricle.jpg"
left_rectcircle_cdi <- "left rect cricle CDI.jpg"
right_rectcircle <- "right rect cricle.jpg"
house <- "home.png"
hospital <- "hospital.png"
elder_man <- "elder_m.jpg"
elder_woman <- "elder_w.png"
adult_man <- "adult_m.png"
adult_man25 <- "adult_m25.png"
adult_man50 <- "adult_m50.png"
adult_man75 <- "adult_m75.png"
adult_woman <- "adult_w.png"
baby <- "baby.png"
people <- "people.png"

##############################
#   FIXED ELEMENTS           #
##############################

#EMPTY PLOT 
empty_plot <- data.frame()

#BACKGROUND ELEMENTS
create_element <- function (image_file, output) {
  r_object <- image_read(image_file)
  r_object <- ggplot(empty_plot) + draw_image(r_object)
  assign(output, r_object, env = .GlobalEnv)
}

create_house <- function (image_file, output) {
  r_object <- image_read(image_file)
  r_object <- image_fill(r_object, infographics_colour, point = "+500+500", fuzz = 90)
  r_object <- ggplot(empty_plot) + draw_image(r_object)
  assign(output, r_object, env = .GlobalEnv)
}

create_hospital <- function (image_file, output) {
  r_object <- image_read(image_file)
  r_object <- image_fill(r_object, infographics_colour2, point = "+60+250", fuzz = 90)
  r_object <- image_fill(r_object, infographics_colour2, point = "+230+450", fuzz = 90)
  r_object <- image_fill(r_object, infographics_colour2, point = "+410+450", fuzz = 90)
  r_object <- image_fill(r_object, infographics_colour2, point = "+580+450", fuzz = 90)
  r_object <- image_fill(r_object, infographics_colour2, point = "+750+450", fuzz = 90)
  r_object <- image_fill(r_object, infographics_colour2, point = "+230+590", fuzz = 90)
  r_object <- image_fill(r_object, infographics_colour2, point = "+410+590", fuzz = 90)
  r_object <- image_fill(r_object, infographics_colour2, point = "+580+590", fuzz = 90)
  r_object <- image_fill(r_object, infographics_colour2, point = "+750+590", fuzz = 90)
  r_object <- image_fill(r_object, infographics_colour2, point = "+230+720", fuzz = 90)
  r_object <- image_fill(r_object, infographics_colour2, point = "+750+720", fuzz = 90)
  r_object <- ggplot(empty_plot) + draw_image(r_object)
  assign(output, r_object, env = .GlobalEnv)
}

create_adultm <- function (image_file, output) {
  r_object <- image_read(image_file)
  r_object <- image_fill(r_object, background_colour, point = "+4+2", fuzz = 0)
  r_object <- image_fill(r_object, infographics_colour, point = "+40+20", fuzz = 50)
  r_object <- image_fill(r_object, infographics_colour, point = "+50+85", fuzz = 50)
  r_object <- ggplot(empty_plot) + draw_image(r_object)
  assign(output, r_object, env = .GlobalEnv)
}


create_adultm25 <- function (image_file, output) {
  r_object <- image_read(image_file)
  r_object <- image_fill(r_object, background_colour, point = "+4+2", fuzz = 0)
  r_object <- image_fill(r_object, infographics_colour, point = "+30+200", fuzz = 50)
  r_object <- image_fill(r_object, infographics_colour, point = "+60+200", fuzz = 50)
  r_object <- ggplot(empty_plot) + draw_image(r_object)
  assign(output, r_object, env = .GlobalEnv)
}

create_adultm50 <- function (image_file, output) {
  r_object <- image_read(image_file)
  r_object <- image_fill(r_object, background_colour, point = "+4+2", fuzz = 0)
  r_object <- image_fill(r_object, infographics_colour, point = "+10+120", fuzz = 50)
  r_object <- image_fill(r_object, infographics_colour, point = "+80+120", fuzz = 50)
  r_object <- image_fill(r_object, infographics_colour, point = "+40+120", fuzz = 50)
  r_object <- ggplot(empty_plot) + draw_image(r_object)
  assign(output, r_object, env = .GlobalEnv)
}

create_adultm75 <- function (image_file, output) {
  r_object <- image_read(image_file)
  r_object <- image_fill(r_object, background_colour, point = "+4+2", fuzz = 0)
  r_object <- image_fill(r_object, infographics_colour, point = "+50+100", fuzz = 50)
  r_object <- ggplot(empty_plot) + draw_image(r_object)
  assign(output, r_object, env = .GlobalEnv)
}

create_adultw <- function (image_file, output) {
  r_object <- image_read(image_file)
  r_object <- image_fill(r_object, background_colour, point = "+10+10", fuzz = 0)
  r_object <- image_fill(r_object, infographics_colour2, point = "+60+20", fuzz = 50)
  r_object <- image_fill(r_object, infographics_colour2, point = "+60+100", fuzz = 50)
  r_object <- ggplot(empty_plot) + draw_image(r_object)
  assign(output, r_object, env = .GlobalEnv)
}

create_elderm <- function (image_file, output) {
  r_object <- image_read(image_file)
  r_object <- image_fill(r_object, background_colour, point = "+14+14", fuzz = 10)
  r_object <- image_fill(r_object, infographics_colour, point = "+100+25", fuzz = 50)
  r_object <- image_fill(r_object, infographics_colour, point = "+100+100", fuzz = 50)
  r_object <- ggplot(empty_plot) + draw_image(r_object)
  assign(output, r_object, env = .GlobalEnv)
}

create_elderw <- function (image_file, output) {
  r_object <- image_read(image_file)
  r_object <- image_fill(r_object, background_colour, point = "+14+14", fuzz = 10)
  r_object <- image_fill(r_object, infographics_colour2, point = "+250+250", fuzz = 50)
  r_object <- image_fill(r_object, infographics_colour2, point = "+250+50", fuzz = 50)
  r_object <- ggplot(empty_plot) + draw_image(r_object)
  assign(output, r_object, env = .GlobalEnv)
}

create_people <- function (image_file, output) {
  r_object <- image_read(image_file)
  r_object <- image_fill(r_object, background_colour, point = "+50+50", fuzz = 10)
  r_object <- image_fill(r_object, infographics_colour, point = "+75+160", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+40+270", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+150+115", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+170+200", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+140+300", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+250+100", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+255+185", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+250+260", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+260+370", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+340+110", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+340+200", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+380+300", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+440+160", fuzz = 70)
  r_object <- image_fill(r_object, infographics_colour, point = "+460+260", fuzz = 70)
  r_object <- ggplot(empty_plot) + draw_image(r_object)
  assign(output, r_object, env = .GlobalEnv)
}

create_baby <- function (image_file, output, fill) {
  r_object <- image_read(image_file)
  r_object <- image_fill(r_object, color = fill, point = "+260+100", fuzz = 70)
  r_object <- image_fill(r_object, color = fill, point = "+260+230", fuzz = 70)
  r_object <- image_fill(r_object, color = fill, point = "+175+360", fuzz = 70)
  r_object <- image_fill(r_object, color = fill, point = "+340+360", fuzz = 70)
  r_object <- ggplot(empty_plot) + draw_image(r_object)
  assign(output, r_object, env = .GlobalEnv)
}

create_element(logo_location, "insert_logo")
create_element(left_squarecircle, "insert_left_squarecircle")
create_element(right_squarecircle, "insert_right_squarecircle")
create_element(left_rectcircle, "insert_left_rectcircle")
create_element(left_rectcircle_cdi, "insert_left_rectcircle_cdi")
create_element(right_rectcircle, "insert_right_rectcircle")

create_people(people, "insert_people")

create_adultm(adult_man, "insert_am")
create_adultw(adult_woman, "insert_aw")
create_elderm(elder_man, "insert_em")
create_elderw(elder_woman, "insert_ew")
create_baby(baby, "insert_baby1", infographics_colour)
create_baby(baby, "insert_baby2", infographics_colour2)

create_adultm25(adult_man25, "insert_am25")
create_adultm50(adult_man50, "insert_am50")
create_adultm75(adult_man75, "insert_am75")

#TO BE DELTED 4 below?
low_male <- insert_am
low_female <- insert_aw
high_male <- insert_em
high_female <- insert_ew

create_house(house, "insert_house")
create_hospital(hospital, "insert_hospital")

crown_copyright <- paste0("\U00A9 Crown copyright ", this_year)

##############################
#        FIGURES             #
##############################

#top
top_plot <- ggplot(plot_data, aes(x = years, y = rate, group = 1)) +
  geom_line(size = 2, colour = infographics_colour) + 
  ylim(0,max(plot_data$rate)) + 
  xlab("Financial Year") + ylab("Rate, per \n1000,000 population") +
  labs(title = plot_name) +
  theme(axis.title = element_text(color = infographics_colour)) +
  theme(plot.title = element_text(color = infographics_colour, face = "bold", size = 12 )) +
  theme(plot.subtitle = element_text(color = infographics_colour, size = 12, hjust = 0.5 )) +
  theme (axis.text = element_text(color = infographics_colour)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(panel.grid.major.y = element_line(color = infographics_colour)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))  +
  theme(plot.background =  element_rect(fill = background_colour)) +
  if (info_type == 6){labs(subtitle = tolower(title_name2))} 

ggsave(filename = "top.png", plot = top_plot, device = "png", path = getwd(), width = 5, height = 3)
create_element("top.png", "insert_plot")


#waffle plot
if (waffle_decimals == 0) {
  insert_extra <- NULL
} else if(waffle_decimals < 0.35) {
  insert_extra <- insert_am25
} else if (waffle_decimals >= 0.35 & waffle_decimals < 0.65) {
  insert_extra <- insert_am50
} else if (waffle_decimals >= 0.65) {
  insert_extra <- insert_am75
}

waffle_element <- insert_am + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  xlab(NULL) + ylab(NULL)

if (is.null(insert_extra)) {
  waffle_plot <- eval(parse(text = paste("plot_grid(", 
                                         paste(rep("waffle_element", times = get_waffle_integers), collapse = ","), ", scale = 1.1)")))
}

if (!is.null(insert_extra)) {
waffle_extra <- insert_extra + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  xlab(NULL) + ylab(NULL)

waffle_plot <- eval(parse(text = paste("plot_grid(", 
                        paste(rep("waffle_element", times = get_waffle_integers), collapse = ","), ", waffle_extra, scale = 1.1)")))
}

#bottom left
doughnut_plot <-
  ggplot(doughnut_data, aes(x = 2.5, y = doughnut_data$count)) +
  geom_bar(width = 0.7, stat = "identity", color = background_colour, fill = doughnut_palette) +
  coord_polar(theta = 'y', start = 0) +
  scale_y_continuous(breaks=cumsum(doughnut_data$count) - doughnut_data$count / 2) +
  geom_text(aes(x = 2.5, label = paste0(count, "%")), size = 2.5, color = "white", position = position_stack(vjust = 0.6)) +
  geom_text(aes( x = 4, label = source), size = 3, color = infographics_colour, position = position_stack(vjust = 0.5)) +
  xlim(0.8, 4.5) + 
  theme_void() +
  theme(legend.position = "none")

################################
################################
##                            ##
##        CREATE PDF          ##
##                            ##
################################
################################

create_infographics <- function(output_name = "Infographics.pdf", output_path = "~") {

#PDF SETUP
temp_wd = getwd()
setwd(output_path)
grDevices::cairo_pdf(output_name, onefile = T, width = 8.27, height = 11.69)
  
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 117, ncol = 83, widths = unit(0.1, "inches"), heights = unit(0.1, "inches"))))
par(col = "blue")

#ELEMENTS
#logo
print(insert_logo, vp = useful::vplayout(x = 1:20, y = 1:25))

#title
title_length_combined <- str_length(paste(title_name1, title_name2))
if (title_length_combined <= 19) {
  #short title
  if (italize_title == T) {
    grid::grid.text(title_name1, just = "left", y = unit(0.93, "npc"), x = unit(0.27, "npc"), gp = gpar(col = infographics_colour, fontsize = 44, fontface = "bold.italic"))
  } else {
    grid::grid.text(title_name1, just = "left", y = unit(0.93, "npc"), x = unit(0.27, "npc"), gp = gpar(col = infographics_colour, fontsize = 44, fontface = "bold"))
  }
  grid::grid.text(title_name2, just = "left", y = unit(0.93, "npc"), x = unit(0.53, "npc"), gp = gpar(col = infographics_colour, fontsize = 44, fontface = "bold"))
  grid::grid.text(title_place, just = "left", y = unit(0.85, "npc"), x = unit(0.27, "npc"), gp = gpar(col = infographics_colour, fontsize = 44,  fontface = "bold"))
  grid::grid.text(title_year, just = "left", y = unit(0.85, "npc"), x = unit(0.6, "npc"), gp = gpar(col = infographics_colour, fontsize = 44,  fontface = "bold"))
} else {
  # long title
  if (str_length(title_name1) <= 16) {
    #long title with short first substring
    if (italize_title == T) {
      grid::grid.text(title_name1, just = "left", y = unit(0.95, "npc"), x = unit(0.35, "npc"), gp = gpar(col = infographics_colour, fontsize = 48, fontface = "bold.italic"))
    } else {
      grid::grid.text(title_name1, just = "left", y = unit(0.95, "npc"), x = unit(0.35, "npc"), gp = gpar(col = infographics_colour, fontsize = 48, fontface = "bold"))
    }
    grid::grid.text(title_name2, just = "left", y = unit(0.9, "npc"), x = unit(0.35, "npc"), gp = gpar(col = infographics_colour, fontsize = 40, fontface = "bold"))
    grid::grid.text(title_place, just = "left", y = unit(0.85, "npc"), x = unit(0.35, "npc"), gp = gpar(col = infographics_colour, fontsize = 40,  fontface = "bold"))
    grid::grid.text(title_year, just = "left", y = unit(0.85, "npc"), x = unit(0.65, "npc"), gp = gpar(col = infographics_colour, fontsize = 40,  fontface = "bold"))
  } else {
    #long title with longer first substring
    if (italize_title == T) {
      grid::grid.text(title_name1, just = "left", y = unit(0.93, "npc"), x = unit(0.28, "npc"), gp = gpar(col = infographics_colour, fontsize = 30, fontface = "bold.italic"))
    } else { 
      grid::grid.text(title_name1, just = "left", y = unit(0.93, "npc"), x = unit(0.28, "npc"), gp = gpar(col = infographics_colour, fontsize = 30, fontface = "bold"))
    }
    grid::grid.text(title_name2, just = "left", y = unit(0.89, "npc"), x = unit(0.28, "npc"), gp = gpar(col = infographics_colour, fontsize = 30, fontface = "bold"))
    grid::grid.text(title_place, just = "left", y = unit(0.85, "npc"), x = unit(0.28, "npc"), gp = gpar(col = infographics_colour, fontsize = 30,  fontface = "bold"))
    grid::grid.text(title_year, just = "left", y = unit(0.85, "npc"), x = unit(0.55, "npc"), gp = gpar(col = infographics_colour, fontsize = 30,  fontface = "bold"))
  }}

#background shapes
print(insert_left_rectcircle, vp = vplayout(x = 15:55, y = 1:83))
print(insert_right_rectcircle, vp = vplayout(x = 40:85, y = 1:83))

#top 
grid::grid.text("Overall rate", just = "left", y = unit(0.79, "npc"), x = unit(0.1, "npc"), gp = gpar(col = infographics_colour, fontsize = 22, fontface = "bold"))
if (rate_number <= 9) {
  grid::grid.text(rate_number, just = "left", y = unit(0.75, "npc"), x = unit(0.1, "npc"), gp = gpar(col = infographics_colour2, fontsize = 60, fontface = "bold"))
} else if (rate_number <= 99) {
  grid::grid.text(rate_number, just = "left", y = unit(0.75, "npc"), x = unit(0.07, "npc"), gp = gpar(col = infographics_colour2, fontsize = 60, fontface = "bold"))
} else {
  grid::grid.text(rate_number, just = "left", y = unit(0.75, "npc"), x = unit(0.06, "npc"), gp = gpar(col = infographics_colour2, fontsize = 48, fontface = "bold"))
}
grid::grid.text("people out of every", just = "left", y = unit(0.75, "npc"), x = unit(0.2, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
grid::grid.text("100,000", just = "left", y = unit(0.7, "npc"), x = unit(0.2, "npc"), gp = gpar(col = infographics_colour2, fontsize = 38))
grid::grid.text("will acquire an", just = "left", y = unit(0.67, "npc"), x = unit(0.2, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))

if (title_length_combined <= 19) {
  if (italize_title == T) {
    grid::grid.text(paste(title_name1), just = "left", y = unit(0.65, "npc"), x = unit(0.2, "npc"), gp = gpar(col = infographics_colour, fontsize = 12, fontface = "italic"))
    grid::grid.text(paste(tolower(title_name2)), just = "left", y = unit(0.65, "npc"), x = unit(0.26, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
    } else if (italize_title == F) {
  grid::grid.text(paste(title_name1), just = "left", y = unit(0.65, "npc"), x = unit(0.2, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
  grid::grid.text(paste(tolower(title_name2)), just = "left", y = unit(0.65, "npc"), x = unit(0.26, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
}
} else {
  if (italize_title == T) {
     grid::grid.text(paste(title_name1), just = "left", y = unit(0.65, "npc"), x = unit(0.2, "npc"), gp = gpar(col = infographics_colour, fontsize = 12, fontface = "italic"))
     grid::grid.text(paste(tolower(title_name2)), just = "left", y = unit(0.63, "npc"), x = unit(0.2, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
  } else if (italize_title == F) {
     grid::grid.text(paste(title_name1), just = "left", y = unit(0.65, "npc"), x = unit(0.2, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
     grid::grid.text(paste(tolower(title_name2)), just = "left", y = unit(0.63, "npc"), x = unit(0.2, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
  }
}

if (rate_number <= 49) {
  print(waffle_plot, vp = useful::vplayout(x = 35:45, y = 6:16))
} else {
  print(insert_people, vp = useful::vplayout(x = 35:45, y = 6:16))
}

print(insert_plot, vp = useful::vplayout(x = 25:46, y = 35:70))

#middle 
grid::grid.text(paste("Risk greater among", rhigh_factor), just = "left", y = unit(0.55, "npc"), x = unit(0.2, "npc"), gp = gpar(col = infographics_colour, fontsize = 28, fontface = "bold"))

grid::grid.text(paste(str_to_title(rlow_factor2), "male rate"), just = "left", y = unit(0.51, "npc"), x = unit(0.12, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
if (rlow_factor2 == "infant") {
  print(insert_baby1, vp = vplayout(x = 58:68, y = 5:15))
} else if (rlow_factor2 == "youth") {
  print(insert_am, vp = vplayout(x = 60:68, y = 5:15))
} else if (rlow_factor2 == "adult") {
  print(insert_am, vp = vplayout(x = 58:68, y = 5:15))
} else if (rlow_factor2 == "elderly") {
  print(insert_em, vp = vplayout(x = 58:68, y = 5:15))
}

grid::grid.text(rlow_malenumber, just = "left", y = unit(0.48, "npc"), x = unit(0.15, "npc"), gp = gpar(col = infographics_colour, fontsize = 36, fontface = "bold"))
grid::grid.text(paste(rlow_factor2, "male"), just = "left", y = unit(0.45, "npc"), x = unit(0.15, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
grid::grid.text("out of every", just = "left", y = unit(0.43, "npc"), x = unit(0.15, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
grid::grid.text("100,000", just = "left", y = unit(0.41, "npc"), x = unit(0.15, "npc"), gp = gpar(col = infographics_colour, fontsize = 20))
grid::grid.text(rlow_age, just = "left", y = unit(0.39, "npc"), x = unit(0.15, "npc"), gp = gpar(col = infographics_colour, fontsize = 8))

grid::grid.text(paste(str_to_title(rlow_factor2), "female rate"), just = "left", y = unit(0.51, "npc"), x = unit(0.33, "npc"), gp = gpar(col = infographics_colour2, fontsize = 12))

if (rlow_factor2 == "infant") {
  print(insert_baby2, vp = vplayout(x = 58:68, y = 23:33))
} else if (rlow_factor2 == "youth") {
  print(insert_aw, vp = vplayout(x = 60:68, y = 23:33))
} else if (rlow_factor2 == "adult") {
  print(insert_aw, vp = vplayout(x = 58:68, y = 23:33))
} else if (rlow_factor2 == "elderly") {
  print(insert_ew, vp = vplayout(x = 58:68, y = 23:33))
}
grid::grid.text(rlow_femnumber, just = "left", y = unit(0.48, "npc"), x = unit(0.37, "npc"), gp = gpar(col = infographics_colour2, fontsize = 36, fontface = "bold"))
grid::grid.text(paste(rlow_factor2, "female"), just = "left", y = unit(0.45, "npc"), x = unit(0.37, "npc"), gp = gpar(col = infographics_colour2, fontsize = 12))
grid::grid.text("out of every", just = "left", y = unit(0.43, "npc"), x = unit(0.37, "npc"), gp = gpar(col = infographics_colour2, fontsize = 12))
grid::grid.text("100,000", just = "left", y = unit(0.41, "npc"), x = unit(0.37, "npc"), gp = gpar(col = infographics_colour2, fontsize = 20))
grid::grid.text(rlow_age, just = "left", y = unit(0.39, "npc"), x = unit(0.37, "npc"), gp = gpar(col = infographics_colour2, fontsize = 8))

grid::grid.text(paste(str_to_title(rhigh_factor2), "male rate"), just = "left", y = unit(0.51, "npc"), x = unit(0.58, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
if (rhigh_factor2 == "infant") {
  print(insert_baby1, vp = vplayout(x = 58:68, y = 42:52))
} else if (rhigh_factor2 == "youth") {
  print(insert_am, vp = vplayout(x = 60:68, y = 42:52))
} else if (rhigh_factor2 == "adult") {
  print(insert_am, vp = vplayout(x = 58:68, y = 42:52))
} else if (rhigh_factor2 == "elderly") {
  print(insert_em, vp = vplayout(x = 58:68, y = 42:52))
}

grid::grid.text(rhigh_malenumber, just = "left", y = unit(0.48, "npc"), x = unit(0.6, "npc"), gp = gpar(col = infographics_colour, fontsize = 36, fontface = "bold"))
grid::grid.text(paste(rhigh_factor2, "male"), just = "left", y = unit(0.45, "npc"), x = unit(0.6, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
grid::grid.text("out of every", just = "left", y = unit(0.43, "npc"), x = unit(0.6, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))
grid::grid.text("100,000", just = "left", y = unit(0.41, "npc"), x = unit(0.6, "npc"), gp = gpar(col = infographics_colour, fontsize = 20))
grid::grid.text(rhigh_age, just = "left", y = unit(0.39, "npc"), x = unit(0.6, "npc"), gp = gpar(col = infographics_colour, fontsize = 8))

grid::grid.text(paste(str_to_title(rhigh_factor2), "female rate"), just = "left", y = unit(0.51, "npc"), x = unit(0.75, "npc"), gp = gpar(col = infographics_colour2, fontsize = 12))
if (rhigh_factor2 == "infant") {
  print(insert_baby2, vp = vplayout(x = 58:68, y = 59:69))
} else if (rhigh_factor2 == "youth") {
  print(insert_aw, vp = vplayout(x = 60:68, y = 59:69))
} else if (rhigh_factor2 == "adult") {
  print(insert_aw, vp = vplayout(x = 58:68, y = 59:69))
} else if (rhigh_factor2 == "elderly") {
  print(insert_ew, vp = vplayout(x = 58:68, y = 59:69))
}

grid::grid.text(rhigh_femnumber, just = "left", y = unit(0.48, "npc"), x = unit(0.81, "npc"), gp = gpar(col = infographics_colour2, fontsize = 36, fontface = "bold"))
grid::grid.text(paste(rhigh_factor2, "female"), just = "left", y = unit(0.45, "npc"), x = unit(0.81, "npc"), gp = gpar(col = infographics_colour2, fontsize = 12))
grid::grid.text("out of every", just = "left", y = unit(0.43, "npc"), x = unit(0.81, "npc"), gp = gpar(col = infographics_colour2, fontsize = 12))
grid::grid.text("100,000", just = "left", y = unit(0.41, "npc"), x = unit(0.81, "npc"), gp = gpar(col = infographics_colour2, fontsize = 20))
grid::grid.text(rhigh_age, just = "left", y = unit(0.39, "npc"), x = unit(0.81, "npc"), gp = gpar(col = infographics_colour2, fontsize = 8))


if (info_type %in% c(2:6)) {
  
#element
  print(insert_left_squarecircle, vp = vplayout(x = 75:117, y = 1:46))
  print(insert_right_squarecircle, vp = vplayout(x = 75:117, y = 38:83))
  
#bottom left
grid::grid.text(common_source, just = "left", y = unit(0.3, "npc"), x = unit(0.07, "npc"), gp = gpar(col = infographics_colour, fontsize = 22))
grid::grid.text("infections are the most common source", just = "left", y = unit(0.27, "npc"), x = unit(0.07, "npc"), gp = gpar(col = infographics_colour, fontsize = 12))

print(doughnut_plot, vp = vplayout(x = 85:115, y = 7:37))

grid::grid.text(crown_copyright, just = "left", y = unit(0.03, "npc"), x = unit(0.07, "npc"), gp = gpar(col = infographics_colour, fontsize = 5))

#bottom right
if (is.null(most_cases)) {
  grid::grid.text("Cases are equally", just = "right", y = unit(0.31, "npc"), x = unit(0.92, "npc"), gp = gpar(col = infographics_colour, fontsize = 14))
  grid::grid.text("divided amongst", just = "right", y = unit(0.29, "npc"), x = unit(0.92, "npc"), gp = gpar(col = infographics_colour, fontsize = 14))
  grid::grid.text("community- and hospital-onsets", just = "right", y = unit(0.27, "npc"), x = unit(0.92, "npc"), gp = gpar(col = infographics_colour, fontsize = 14))
} else {
  grid::grid.text("Most cases are", just = "right", y = unit(0.31, "npc"), x = unit(0.92, "npc"), gp = gpar(col = infographics_colour, fontsize = 18))
  grid::grid.text(most_cases, just = "right", y = unit(0.28, "npc"), x = unit(0.92, "npc"), gp = gpar(col = infographics_colour, fontsize = 24, fontface = "bold"))
}


print(insert_house, vp = vplayout(x = 87:102, y = 47:62))
grid::grid.text(paste0(community_percent, "%"), just = "left", y = unit(0.11, "npc"), x = unit(0.61, "npc"), gp = gpar(col = infographics_colour, fontsize = 28, fontface = "bold"))
grid::grid.text("< 2 days", just = "left", y = unit(0.08, "npc"), x = unit(0.61, "npc"), gp = gpar(col = infographics_colour, fontsize = 14, fontface = "bold"))

print(insert_hospital, vp = vplayout(x = 87:102, y = 62:77))
grid::grid.text(paste0(hospital_percent, "%"), just = "left", y = unit(0.11, "npc"), x = unit(0.8, "npc"), gp = gpar(col = infographics_colour2, fontsize = 28, fontface = "bold"))
grid::grid.text(paste(expression("\U2265 2 days")), just = "left", y = unit(0.08, "npc"), x = unit(0.8, "npc"), gp = gpar(col = infographics_colour2, fontsize = 14, fontface = "bold"))

grid::grid.text("For full report, please see", just = "left", y = unit(0.05, "npc"), x = unit(0.52, "npc"), gp = gpar(col = infographics_colour, fontsize = 5))
grid::grid.text("https://www.gov.uk/government/statistics/mrsa-mssa-and-e-coli-bacteraemia-and-c-difficile-infection-", just = "left", y = unit(0.04, "npc"), x = unit(0.52, "npc"), gp = gpar(col = infographics_colour, fontsize = 5))
grid::grid.text("annual-epidemiological-commentary", just = "left", y = unit(0.03, "npc"), x = unit(0.52, "npc"), gp = gpar(col = infographics_colour, fontsize = 5))

} else if (info_type == 1) {
  print(insert_left_rectcircle_cdi, vp = vplayout(x = 75:115, y = 1:83))
  
  if (is.null(most_cases_old)) {
    grid::grid.text(paste0(previous_year, " Cases are equally"), just = "left", y = unit(0.31, "npc"), x = unit(0.1, "npc"), gp = gpar(col = infographics_colour, fontsize = 14))
    grid::grid.text("divided amongst community-", just = "left", y = unit(0.29, "npc"), x = unit(0.1, "npc"), gp = gpar(col = infographics_colour, fontsize = 14))
    grid::grid.text("and hospital-onsets", just = "left", y = unit(0.27, "npc"), x = unit(0.1, "npc"), gp = gpar(col = infographics_colour, fontsize = 14))
  } else {
    grid::grid.text(paste0(previous_year, " Most cases are"), just = "left", y = unit(0.31, "npc"), x = unit(0.1, "npc"), gp = gpar(col = infographics_colour, fontsize = 18))
    grid::grid.text(most_cases_old, just = "left", y = unit(0.28, "npc"), x = unit(0.1, "npc"), gp = gpar(col = infographics_colour, fontsize = 24, fontface = "bold"))
  }
  
  print(insert_house, vp = vplayout(x = 87:102, y = 7:22))
  grid::grid.text(paste0(community_percent_old, "%"), just = "left", y = unit(0.11, "npc"), x = unit(0.11, "npc"), gp = gpar(col = infographics_colour, fontsize = 28, fontface = "bold"))
  grid::grid.text("< 2 days", just = "left", y = unit(0.08, "npc"), x = unit(0.11, "npc"), gp = gpar(col = infographics_colour, fontsize = 14, fontface = "bold"))
  
  
  print(insert_hospital, vp = vplayout(x = 87:102, y = 22:37))
  grid::grid.text(paste0(hospital_percent_old, "%"), just = "left", y = unit(0.11, "npc"), x = unit(0.31, "npc"), gp = gpar(col = infographics_colour2, fontsize = 28, fontface = "bold"))
  grid::grid.text(paste(expression("\U2265 4 days")), just = "left", y = unit(0.08, "npc"), x = unit(0.31, "npc"), gp = gpar(col = infographics_colour2, fontsize = 14, fontface = "bold"))
  
  if (is.null(most_cases_new)) {
    grid::grid.text(paste0(as.numeric(this_year)-1,"/", substr(this_year, 3, 4)," Cases are equally"), just = "left", y = unit(0.31, "npc"), x = unit(0.5, "npc"), gp = gpar(col = infographics_colour, fontsize = 14))
    grid::grid.text("divided amongst community-", just = "left", y = unit(0.29, "npc"), x = unit(0.5, "npc"), gp = gpar(col = infographics_colour, fontsize = 14))
    grid::grid.text("and hospital-onsets", just = "left", y = unit(0.27, "npc"), x = unit(0.5, "npc"), gp = gpar(col = infographics_colour, fontsize = 14))
  } else {
    grid::grid.text(paste0(as.numeric(this_year)-1, "/", substr(this_year, 3, 4), " Most cases are"), just = "left", y = unit(0.31, "npc"), x = unit(0.5, "npc"), gp = gpar(col = infographics_colour, fontsize = 18))
    grid::grid.text(most_cases_new, just = "left", y = unit(0.28, "npc"), x = unit(0.5, "npc"), gp = gpar(col = infographics_colour, fontsize = 24, fontface = "bold"))
  }
  
  print(insert_house, vp = vplayout(x = 87:102, y = 42:57))
  grid::grid.text(paste0(community_percent_new, "%"), just = "left", y = unit(0.11, "npc"), x = unit(0.55, "npc"), gp = gpar(col = infographics_colour, fontsize = 28, fontface = "bold"))
  grid::grid.text("< 4 days", just = "left", y = unit(0.08, "npc"), x = unit(0.55, "npc"), gp = gpar(col = infographics_colour, fontsize = 14, fontface = "bold"))
  
  
  print(insert_hospital, vp = vplayout(x = 87:102, y = 57:72))
  grid::grid.text(paste0(hospital_percent_new, "%"), just = "left", y = unit(0.11, "npc"), x = unit(0.73, "npc"), gp = gpar(col = infographics_colour2, fontsize = 28, fontface = "bold"))
  grid::grid.text(paste(expression("\U2265 4 days")), just = "left", y = unit(0.08, "npc"), x = unit(0.73, "npc"), gp = gpar(col = infographics_colour2, fontsize = 14, fontface = "bold"))
  
  
  grid::grid.text(crown_copyright, just = "left", y = unit(0.05, "npc"), x = unit(0.07, "npc"), gp = gpar(col = infographics_colour, fontsize = 5))
  
  grid::grid.text("For full report, please see", just = "left", y = unit(0.06, "npc"), x = unit(0.22, "npc"), gp = gpar(col = infographics_colour, fontsize = 5))
  grid::grid.text("https://www.gov.uk/government/statistics/mrsa-mssa-and-e-coli-bacteraemia-and-c-difficile-infection-annual-epidemiological-commentary", just = "left", y = unit(0.05, "npc"), x = unit(0.22, "npc"), gp = gpar(col = infographics_colour, fontsize = 5))
 # grid::grid.text("annual-epidemiological-commentary", just = "left", y = unit(0.03, "npc"), x = unit(0.52, "npc"), gp = gpar(col = infographics_colour, fontsize = 5))
  
  
  
}
#CREATE PDF AND CLOSE DEVICE
setwd(temp_wd)
print(paste0(output_name, " was saved in ", path.expand(output_path)))
grDevices::dev.off()
}

#create_infographics("Test.pdf", "C:/Users/Piotr.Patrzylas/Desktop/")