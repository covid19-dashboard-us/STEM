#' Map plotting for Spatio-Temporal Epidemic Model (STEM)
#'
#' @import plotly
#' @import BPST
#'
plot.map <- function(coef, VT, limits = c(-6, 6), trunc= c(-2, 2), main=""){

  col2 <- colorRampPalette(c("#053061", "#2166AC", "#4393C3", "#92C5DE",
    "#D1E5F0", "#FFFFFF", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F"))
  load(file = "data/VT.true.rda")
  V.true = as.matrix(VT.true$V)
  Tr.true = as.matrix(VT.true$Tr)
  load(file = "data/USbb2.rda")
  bb2 = USbb2
  S1b = cbind(min(bb2[,1]), max(bb2[,1]))
  S2b = cbind(min(bb2[,2]), max(bb2[,2]))

  dist=0.25
  uu = seq(S1b[1], S1b[2], dist);
  vv = seq(S2b[1], S2b[2], dist);
  n1 = length(uu); n2 = length(vv)
  u = rep(uu,n2); v = rep(vv,rep(n1,n2));
  uvpop = cbind(u,v)
  ind.true = inVT(V.true, Tr.true, u, v)$ind

  file.name = paste0("data/Bplot.VT", VT, ".rda")
  load(file.name)
  Q2 = B0$Q2
  BQ2 = matrix(NA,nrow=n1*n2,ncol=ncol(Q2))
  BQ2[B0$Ind.inside,] = as.matrix(B0$B%*%Q2)

  beta = data.frame(x = u, y = v, z = BQ2%*%coef)
  beta$z[beta$z < trunc[1]] = trunc[1]
  beta$z[beta$z > trunc[2]] = trunc[2]
  beta$z[ind.true==0]=NA
  beta.mtx = matrix(beta$z,n1,n2)
  x.mtx = matrix(beta$x,n1,n2)
  y.mtx = matrix(beta$y,n1,n2)
  beta.mtx = t(beta.mtx)

  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = 'white', # toRGB("gray95"),
    subunitcolor = 'black', # toRGB("gray85"),
    countrycolor = 'black', # toRGB("gray85"),
    countrywidth = 1.5,
    subunitwidth = 1.5
  )
  fig <- plot_geo(beta[!is.na(beta$z),], lat = ~y, lon = ~x) %>%
    add_markers(color = ~z, opacity = 0.9, colors = col2(200),
    text = ~paste('Value:',as.character(round(z,2))))
  fig = fig %>% colorbar(limits = limits, title = "") %>%
    layout(title = main, geo = g)
  return(fig)
}
