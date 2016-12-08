#mapping crime in san jose, ca

library(ggmap)

sj <- get_map(location='san jose',zoom=1)
ggmap(sj)
sj