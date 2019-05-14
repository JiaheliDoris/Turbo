#' Fitted Values from Model with \code{nterm} Terms
#'
#' Fitted values from model with \code{nterm} terms, based on truncated basis functions.
#' @param x     a numerical vector, viewed as predictors in training sample.
#' @param y1    a numerical vector, viewed as responses in training sample.
#' @param nterm the number of terms to be included in the fitting model.
#' @return      \code{regsubsets.fitted.values} produces a vector of fitted values, obtained from a model with \code{nterm} terms.
#' @details     Based on the n × n design matrix for the set of truncated basis functions, \code{regsubsets.fitted.values} computes the fitted values for the model with \code{nterm} terms.
#' @export regsubsets.fitted.values
#' @importFrom dplyr "%>%"
#' @examples
#' library(dplyr)
#' x <- rnorm(50)
#' y <- sin(x) + rnorm(50,1,2)
#' nterm <- 7
#' regsubsets.fitted.values(x,y,nterm)
#’

regsubsets.fitted.values <- function(x, y1, nterm){
  x <- sort(x)
  y1 <- y1[order(x)]
  x_des <- truncated.power.design.matrix(x)
  des <- y1 %>%
    cbind(as.data.frame(x_des)) %>%
    as.data.frame()
  colnames(des)[1] <- c("y1")
  n <- length(x)
  regsubsets.out <- leaps::regsubsets(y1 ~., des, nvmax = n, intercept = FALSE, really.big = TRUE, method = "forward")
  choose_coef <- names(coef(regsubsets.out, nterm))
  choose_data <- des %>% select("y1", choose_coef)
  lm <- lm(y1 ~., choose_data)
  return(lm$fitted.values)
}
