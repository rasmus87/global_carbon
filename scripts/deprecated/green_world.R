# Remake the green world plot
# I.e. which climate zones are controlled by animals


MAT <- seq(-15, 30, length.out = 100)
MAP <- seq(0, 4500, length.out = 100)

xy <- expand.grid(MAT, MAP)

MAP[MAP > 7.143 * MAT + 286]
MAP[MAP < 1.469 * MAT^2 + 81.665 * MAT + 475]

plot(MAT ~ MAP)


y1 = 7.143 * MAT + 286
y2 = -1.469 * MAT^2 + 81.665 * MAT + 475

plot(xy)
lines(y1 ~ MAT)
lines(y2 ~ MAT)

xy <- as_tibble(xy)
colnames(xy) <- c("MAT", "MAP")
ggplot(xy, aes(MAT, MAP, fill = MAP > 7.143 * MAT + 286 & (MAP) < 1.469 * (MAT)^2 + 81.665 * MAT + 475)) +
  geom_tile()
  