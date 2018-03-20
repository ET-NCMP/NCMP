# Correction of climdex.pcic function to correctly calculate percent days diagnostics in the
# presence of too few years. This was generating warnings, and causing an unrealistic negative
# bias in the estimated values (a bias *is* expected, but this *was* a miscalculation)
# For ease of use, also circumventing most climdex.pcic wrapper functions

percent.days.op.threshold.mod <- function(ci,freq,type)
    {
    stopifnot(inherits(ci,"climdexInput"))
    freq <- match.arg(freq,c("monthly","annual"))
    type <- match.arg(type,c("TN10p","TN90p","TX10p","TX90p"))
    fo <- switch(type,
        TN10p = c("tmin","q10","<"), TN90p = c("tmin","q90",">"),
        TX10p = c("tmax","q10","<"), TX90p = c("tmax","q90",">"))
    vn <- fo[1]
    qn <- fo[2]
    f <- match.fun(fo[3])
    dat <- f(ci@data[[vn]], ci@quantiles[[vn]]$outbase[[qn]][ci@jdays])
    inset <- ci@dates >= ci@base.range[1] & ci@dates <= ci@base.range[2]
    if (sum(inset) > 0L && length(ci@dates) >= 720L) {
        temp.base <- ci@data[[vn]][inset]
        years.base <- as.POSIXlt(ci@dates[inset])$year + 1900L
        years.base.range <- range(years.base)
        byrs <- years.base.range[2] - years.base.range[1] + 1L
        base.thresholds <- ci@quantiles[[vn]]$inbase[[qn]]
        bdim <- dim(base.thresholds)
        dim(base.thresholds) <- c(bdim[1] * bdim[2], bdim[3])
        yday.byr.indices <- ci@jdays[inset] + (years.base - years.base.range[1]) * bdim[1]
        f.result <- f(rep(temp.base, byrs - 1L), base.thresholds[yday.byr.indices,1:(byrs-1)])
        dim(f.result) <- c(length(yday.byr.indices), byrs - 1L)
        dat[inset] <- rowSums(f.result, na.rm = TRUE)/(byrs - 1)
    }
    dat[is.nan(dat)] <- NA
    na.mask <- ifelse(climdex.pcic:::tapply.fast(dat,ci@date.factors[[freq]],
        function(x) sum(is.na(x)) > ci@max.missing.days[freq]),NA,1)
    ret <- climdex.pcic:::tapply.fast(dat, ci@date.factors[[freq]], mean, na.rm = TRUE) * 
        100 * na.mask
    ret[is.nan(ret)] <- NA
    ret * ci@namasks[[freq]][[vn]]
}
