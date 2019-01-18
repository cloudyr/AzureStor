#' @import AzureRMR
#' @importFrom utils URLencode modifyList packageVersion
NULL

globalVariables("self", "AzureStor")

.AzureStor <- new.env()


.onLoad <- function(libname, pkgname)
{
    .AzureStor$azcopy <- find_azcopy()
}


find_azcopy <- function()
{
    path <- Sys.which("azcopy")
    # we need version 10 or later
    if(path != "")
    {
        ver <- system2(path, "--version", stdout=TRUE)
        if(!grepl("version 1[[:digit:]]", ver, ignore.case=TRUE))
            path <- ""
    }
    unname(path)
}


