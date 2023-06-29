install.packages("ggplot2")
install.packages("ggthemes")
install.packages("ggplot2movies")

library(ggplot2)
library(ggthemes)
library(ggplot2movies)
library(tidyverse)
library(nbastatR)
library(devtools)
devtools::install_github("lbenz730/ncaahoopR")
library(extrafont)
library(cowplot)

# creating court and plotting

circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
    angles = seq(0, 2 * pi, length.out = npoints + 1)
    return(data_frame(x = center[1] + radius * cos(angles),
                      y = center[2] + radius * sin(angles)))
}

# court dimensions and lines

width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.25
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

# court themes

court_themes = list(
    light = list(
        court = 'floralwhite',
        lines = 'black',
        text = '#222222',
        made = 'green',
        missed = '#f8766d',
        hex_border_size = 1,
        hex_border_color = 'black',
    ),
    dark = list(
        court = '#000004',
        lines = '#999999',
        text = '#f0f0f0',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = 'black',
    ),
    ppt = list(
        court = 'gray',
        lines = 'white',
        text = '#f0f0f0',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = 'gray'
    )
)

# function to create court based on given dimensions

plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
    if (use_short_three) {
        three_point_radius = 22
        three_point_side_height = 0
    }

    court_points = data_frame(
        x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
        y = c(height, 0, 0, height, height)
        desc = 'perimeter'
    )

    court_points = bind_rows(court_points, data_frame(
        x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
        y = c(0, key_height, key_height, 0),
        desc = 'outer_key'
    ))

    court_points = bind_rows(court_points , data_frame(
        x = c(-backboard_width / 2, backboard_width / 2),
        y = c(backboard_offset, backboard_offset),
        desc = 'backboard'
    ))

    court_points = bind_rows(court_points, data_frame(
        x = c(-backboard_width / 2, backboard_width / 2),
        y = c(backboard_offset, backboard_offset + neck_length),
        desc = 'neck'
    ))

    foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)

    foul_circle_top = filter(foul_circle, y > key_height) %>%
        mutate(desc = 'foul_circle_top')

    foul_circle_bottom = filter(foul_circle, y < key_height) %>%
        mutate(
            angle = atan((y - key_height) / x) * 180 / pi,
            angle_group = floor((angle - 5.625) / 11.25),
            desc = paste0('foul_circle_bottom_', angle_group)
            )%>%
        filter(angle_group %% 2 == 0) %>%
        select(x, y, desc)

    hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
        mutate(desc = 'hoop')

        # stopping point - https://youtu.be/Af7g95-g4y8?t=329
}
