# this .r script smooths population estimates reported for 5-year age groups into population estimates for 1-year age groups

# togethr, both functions calculate three different interpolation techniques: k-order polynomial, linear, and spline interpolation

# don't run
# install.packages(c("knitr", "tidyverse", "magrittr", "ggplot2", "scales"))



# load 5-year age group counts for the population
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000501
census <- data.frame( # population totals, 2025
  age = c(
    "0-4",
    "5-9",
    "10-14",
    "15-19",
    "20-24",
    "25-29",
    "30-34",
    "35-39",
    "40-44",
    "45-49",
    "50-54",
    "55-59",
    "60-64",
    "65-69",
    "70-74",
    "75-79",
    "80-84",
    "85-89",
    "90-94",
    "95-99"
    ),
  pop = c(
    1871184, 
    2138350, 
    2251628, 
    2319393, 
    2703780, 
    2995647, 
    3175724, 
    3032523, 
    2859631, 
    2593361, 
    2447311, 
    2446446,
    2708208,
    2494286,
    2044533,
    1609542,
    1008273,
     579797,
     275677,
      84078
    )
  )





# 1) function to compute polynomial interpolation ---------------------------------
polynomial <- function(data, k = 1){ # note that a 1-order polynomial is equivalent to linear interpolation
  
  # load pipe locally
  `%>%` <- magrittr::`%>%`

  # stop if the age column is not character
  if (!is.character(data$age)) {
    stop(message("This column must be of class character e.g., '0-4', '5-9', etc."))
  }
  # stop if the column that lists the population is not numeric
  if (!is.numeric(data$pop)) {
    stop(message("This column must be of class numeric."))
  }
  
  # split the string
  string <- stringr::str_split(data$age, pattern = "[-/_;, ]")
  
  # into data frame
  string <- as.data.frame(do.call(rbind, string))

  # recode string to numeric, if string 
  string <- string %>% dplyr::mutate(dplyr::across(dplyr::where(is.character), as.numeric))
  colnames(string) <- c("lo", "hi") 
  
  # join split numeric columns to original data
  data <- cbind(string, data)
  
  # find the center points for the 5-year age brackets i.e., the middle age groups in the numeric coding
  data <- data %>% dplyr::mutate(center = (hi + lo) / 2, bracket_width = abs(hi - lo) + 1 )
    
  # interpolation
  model <- stats::lm(
    # the average population for each age increment in the age bracket
    pop/bracket_width ~ stats::poly(center, degree = k, raw = TRUE), # k-degree polynomial term
    data = data
    )
  print(summary(model)) # display equation
  
  # range
  lo <- min(data$lo); hi <- max(data$hi)
  
  # construct data frame
  newdata <- data.frame( center = seq( from = lo, to = hi, by = 1) )
  
  # estimate census counts by age
  newdata <- newdata %>% dplyr::mutate(pop = stats::predict(model, newdata = .)) %>% dplyr::rename(age = center)
  # newdata <- dplyr::mutate(newdata, pop = round(x = pop, digits = 0)) # round to whole number
  newdata <- dplyr::mutate(newdata, age = lo:hi) # re-scale age distribution
  
  # print age distribution
  print(paste0("age distribution:"))
  print(knitr::kable(newdata)) 
  return(newdata) # return
}
census_poly <- polynomial(data = census, k = 5) 



# function for interpolation ----------------------------------------------------------
interpolationFun <- function(data, method = "spline"){
  
  # load pipe locally
  `%>%` <- magrittr::`%>%`
  
  # stop if age column is not character
  if (!is.character(data$age)) {
    stop(message("The age column must be of class character e.g., '0-4', '5-9', etc."))
  }
    # stop if the column that lists the population is not numeric
  if (!is.numeric(data$pop)) {
    stop(message("This column must be of class numeric."))
  }
  
  # split the string
  string <- stringr::str_split(data$age, pattern = "[-/_;, ]")
  
  # into data frame
  string <- as.data.frame(do.call(rbind, string))

  # recode string to numeric, if string
  string <- string %>% dplyr::mutate(dplyr::across(dplyr::where(is.character), as.numeric))

  # rename columns
  colnames(string) <- c("lo", "hi")
  
  # join
  data <- cbind(string, data)
  
  # find the center points for the 5-year age brackets i.e., the middle age groups in the numeric coding
  data <- data %>% dplyr::mutate(center = (hi + lo) / 2, bracket_width = abs(hi - lo) + 1 )
  
  # range
  lo <- min(data$lo); hi <- max(data$hi)
  
  # construct data frame
  n <- seq(from = lo, to = hi, by = 1)
  
  # the center points
  x <- data$center
  
  # population
  y <- (data$pop / data$bracket_width)
  
  # method
  if (method == "spline") {
  model <- stats::spline( # spline
    x, 
    y, 
    n = length(n), 
    method = "fmm",
    xmin = min(n), 
    xmax = max(n), 
    ties = mean
    )
  } else if (method == "linear") {
    model <- stats::approx( # linear interpolation
        x = x,
        y = y,
        n = length(n),
        method = "linear",
        ties = mean
        )
  }
  newdata <- as.data.frame(model) # coerce to data frame
  newdata <- dplyr::mutate(newdata, x = lo:hi) # rescale age distribution
  newdata <- dplyr::rename(newdata, age = x, pop = y) # rename columns
  # newdata <- dplyr::mutate(newdata, pop = round(x = pop, digits = 0)) # round to whole number
  
  # print age distribution
  print(paste0("age distribution:"))
  print(knitr::kable(newdata)) 
  return(newdata) # return 
}
census_spline <- interpolationFun(data = census, method = "spline")
census_linear <- interpolationFun(data = census, method = "linear")



# compare census estimates by interpolation strategy ---------------------------
test <- function(data, test){
  x <- data[, 2]
  y <- test[, 2]
  r <- stats::cor( # correlation coefficient
    x = x,
    y = y
  )
  print(paste0("correlation coefficient = ", round(x = r, digits = 2)))
}
# run each line separately
test(data = census_poly, test = census_spline)
test(data = census_poly, test = census_linear)
test(data = census_spline , test = census_linear)



# plot the age-graded population estimates
fig_01 <- ggplot2::ggplot() +
  ggplot2::geom_line(data = census_spline, ggplot2::aes(x = age, y = pop, color = "Spline"), size = 1) +
  ggplot2::geom_line(data = census_linear, ggplot2::aes(x = age, y = pop, color = "Linear"), size = 1) +
  ggplot2::geom_line(data = census_poly, ggplot2::aes(x = age, y = pop, color = "Polynomial"), size = 1) +
  ggplot2::scale_color_manual(
    name = "Method", 
    values = c(
      "Spline" = "firebrick1", 
      "Linear" = "blue1",
      "Polynomial" = "forestgreen"
      )
    ) +
  ggplot2::scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1000000, by = 100000), limits = c(0, 1000000),
    labels = scales::label_comma()
    ) +
  ggplot2::labs(
    title = "Population estimates on July 1, 2025, Canada",
    subtitle = "Triangulation of interpolation methods",
    x = "Age", 
    y = "Population"
    ) +
  ggplot2::theme_classic() + 
  ggplot2::theme(
    legend.position = "inside",
    legend.position.inside = c(0.95, 0.95), # x and y coordinates from 0 to 1
    legend.justification = c("right", "top"), # Anchors the legend at those coordinates
    legend.background = ggplot2::element_rect(fill = "white", color = "black")
    )

# function to output high resolution images
output <- function(filename, figure, path = path, width = 10, height = 5){
  ggplot2::ggsave(
    filename,
    figure,
    path = path, 
    width = width, 
    height = height, 
    device = 'png', 
    dpi = 250 # larger DPI increases the size of the plot aesthetics 
    )
}

# output figure
output(
  filename = "fig_01.png",
  figure = fig_01,
  path = "~/Desktop/",
  width = 10,
  height = 5
  )



# close .r script
