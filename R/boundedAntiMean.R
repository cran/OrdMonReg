`BoundedAntiMean` <-
function (y, w, a = NA, b = NA) 
{
    res <- -BoundedIsoMean(-y, w, a = -b, b = -a)
    return(res)
}
