library(rgl)
diabetes <- readRDS("/Users/coleneiderman/Desktop/School/STAT 5650/Diabetes.rds")
diabetes
laqi <- readRDS("/Users/coleneiderman/Desktop/School/STAT 5650/LAQI.rds")
laqi
lol <- readRDS("/Users/coleneiderman/Desktop/School/STAT 5650/Lol.rds")
lol
mull <- readRDS("/Users/coleneiderman/Desktop/School/STAT 5650/mull.rds")
mull
plane <- readRDS("/Users/coleneiderman/Desktop/School/STAT 5650/Plane.rds")
plane

#Setting up Universal X-axis
x <- c(5, 25, 50, 75, 100, 150, 200)
#Diabetes Y-axis
y <- 1:6
#7x6 Accuracy Matrix for voting method
vec <- vector()
for (i in 1:7) {
  for (j in 1:6) {
    vec <- append(vec, diabetes[[i]]$votes[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 6)
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x6 Accuracy Matrix for summing method
vec <- vector()
for (i in 1:7) {
  for (j in 1:6) {
    vec <- append(vec, diabetes[[i]]$sums[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 6)
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x6 Accuracy Matrix for weighted voting method
vec <- vector()
for (i in 1:7) {
  for (j in 1:6) {
    vec <- append(vec, diabetes[[i]]$wvotes[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 6)
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x6 Accuracy Matrix for average coefficient method
vec <- vector()
for (i in 1:7) {
  for (j in 1:6) {
    vec <- append(vec, diabetes[[i]]$combo[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 6)
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#LAQI Plots >:-)
#Initializing Y-Axis
y = 1:40
#7x40 Accuracy Matrix for voting method
vec <- vector()
for (i in 1:7) {
  for (j in 1:40) {
    vec <- append(vec, laqi[[i]]$votes[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 40)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(0.5, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x40 Accuracy Matrix for summing method
vec <- vector()
for (i in 1:7) {
  for (j in 1:40) {
    vec <- append(vec, laqi[[i]]$sums[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 40)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(0.5, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x40 Accuracy Matrix for weighted voting method
vec <- vector()
for (i in 1:7) {
  for (j in 1:40) {
    vec <- append(vec, laqi[[i]]$wvotes[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 40)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(0.5, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x40 Accuracy Matrix for average coefficient method
vec <- vector()
for (i in 1:7) {
  for (j in 1:40) {
    vec <- append(vec, laqi[[i]]$combo[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 40)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(-2, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++", at = c(0, 0.25, 0.5, 0.75, 1))
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#lol plots (:-O
#initializing Y-axis
y = 1:84
#7x40 Accuracy Matrix for voting method
vec <- vector()
for (i in 1:7) {
  for (j in 1:84) {
    vec <- append(vec, lol[[i]]$votes[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 84)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x40 Accuracy Matrix for summing method
vec <- vector()
for (i in 1:7) {
  for (j in 1:84) {
    vec <- append(vec, lol[[i]]$sums[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 84)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x40 Accuracy Matrix for weighted voting method
vec <- vector()
for (i in 1:7) {
  for (j in 1:84) {
    vec <- append(vec, lol[[i]]$wvotes[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 84)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x40 Accuracy Matrix for average coefficient method
vec <- vector()
for (i in 1:7) {
  for (j in 1:84) {
    vec <- append(vec, lol[[i]]$combo[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 84)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(0, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++", at = c(0, 0.25, 0.5, 0.75, 1))
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Mullein plots :'-)
#Initializing Y-axis
y <- 1:29
#7x40 Accuracy Matrix for voting method
vec <- vector()
for (i in 1:7) {
  for (j in 1:29) {
    vec <- append(vec, mull[[i]]$votes[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 29)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(0.5, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x40 Accuracy Matrix for summing method
vec <- vector()
for (i in 1:7) {
  for (j in 1:29) {
    vec <- append(vec, mull[[i]]$sums[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 29)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(0.5, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x40 Accuracy Matrix for weighted voting method
vec <- vector()
for (i in 1:7) {
  for (j in 1:29) {
    vec <- append(vec, mull[[i]]$wvotes[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 29)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(0, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x40 Accuracy Matrix for average coefficient method
vec <- vector()
for (i in 1:7) {
  for (j in 1:29) {
    vec <- append(vec, mull[[i]]$combo[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 29)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(-1, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++", at = c(0, 0.25, 0.5, 0.75, 1))
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Plane data B-)
#initializing Y-axis
y <- 1:17
#7x40 Accuracy Matrix for voting method
vec <- vector()
for (i in 1:7) {
  for (j in 1:17) {
    vec <- append(vec, plane[[i]]$votes[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 17)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(0.5, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x40 Accuracy Matrix for summing method
vec <- vector()
for (i in 1:7) {
  for (j in 1:17) {
    vec <- append(vec, plane[[i]]$sums[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 17)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(0.5, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x40 Accuracy Matrix for weighted voting method
vec <- vector()
for (i in 1:7) {
  for (j in 1:17) {
    vec <- append(vec, plane[[i]]$wvotes[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 17)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(0.6, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++")
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#7x40 Accuracy Matrix for average coefficient method
vec <- vector()
for (i in 1:7) {
  for (j in 1:17) {
    vec <- append(vec, plane[[i]]$combo[[1]][j])
  }
}
mat <- matrix(vec, nrow = 7, ncol = 17)
mat
z <- mat
#Finding points where z is maximized
max_ind <- which(z == max(z), arr.ind = TRUE)
x_coords <- as.list(x[max_ind[, 1]])
y_coords <- as.list(max_ind[, 2])
z_coords <- as.list(rep(as.double(max(z)), length(x_coords)))
#Plotting surface
persp3d(x, y, z, zlim = c(-1, 1), aspect = c(1, 1, 1), color = "ghostwhite", lit = FALSE, 
        polygon_offset = 1, lighting = FALSE, xlab = "# of LMs", 
        ylab = "# of Vars", zlab = "Accuracy", axes = FALSE)
box3d()
axes3d("x++", at = c(5, 25, 50, 75, 100, 150, 200))
axes3d("y+-")
axes3d("z++", at = c(0, 0.25, 0.5, 0.75, 1))
#Plotting facet outlines
persp3d(x, y, z, front = "lines", back = "lines", lit = FALSE, add = TRUE)
#Plotting points where z is maximized
points3d(x_coords, y_coords, z_coords, 
         col = "red", size = 5, add = TRUE)