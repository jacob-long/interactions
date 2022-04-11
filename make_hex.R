library(showtext)
library(jtools)
library(interactions)
library(ggplot2)
library(hexSticker)

font_add("Fantasque Sans Mono", "FantasqueSansMono-Regular.ttf")
font_add("monofur", "monof55.ttf")

# states <- as.data.frame(state.x77)
# states$HSGrad <- states$`HS Grad`
# fit <- lm(Income ~ HSGrad + Murder,
#   data = states)
# p <- effect_plot(model = fit, pred = Murder, x.label = "x", y.label = "y")

set.seed(1000)
x <- rnorm(1000)
z <- rnorm(1000)
y <- x + z + x*z + rnorm(1000, sd=.1)
d <- data.frame(cbind(x, y, z))
p <- interact_plot(fit <- lm(y ~ x * z, data = d), x, z, colors = "CUD Bright")
p <- p +  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
 axis.title.x = element_text(color = "white", size = 5), axis.title.y = element_text(color = "white", size = 5), 
 panel.grid.major = element_line(linetype='longdash', size = 0.25, color = "#e8e8e8b3"), legend.position = "none") + xlim(-1, 3) +
 ylim(-1.75, 5.75)

plot(sticker(p + theme_transparent(), package="interactions", p_family = "Fantasque Sans Mono",
 s_width = 1.25, s_height = 1.2, s_x = 0.93, s_y = 0.83, p_size = 4.5, p_y = 1.53, h_color = "#570008", 
 h_fill = "#844247", url = "interactions.jacob-long.com", u_size = 1.6,
  u_family = "Fantasque Sans Mono", filename = "hex.png", dpi = 500))


# atlantic: #466A9F
# garnet: #73000a
# sandstorm: #FFF2E3
# azalea: #844247
# dark garnet: #570008
