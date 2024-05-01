predict.rlm <- function (object, newdata = NULL, scale = NULL, ...)
{
  ## problems with using predict.lm are the scale and
  ## the QR decomp which has been done on down-weighted values.
  object$qr <- qr(sqrt(object$weights) * object$x)
  predict.lm(object, newdata = newdata, scale = object$s, ...)
}