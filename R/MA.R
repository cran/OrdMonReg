MA <-
function (g, w, A = NA, a, b) 
{
    if (max(is.na(A)) == TRUE) {
        A <- 1:length(g)
    }
    a.up <- max(a[A], na.rm = TRUE)
    b.low <- min(b[A], na.rm = TRUE)
    res <- NA
    if (a.up <= b.low) {
        res <- max(min(sum((g * w)[A])/sum(w[A]), b.low), a.up)
    }
    return(res)
}
