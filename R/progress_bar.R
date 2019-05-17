# custom progress bar
# based on httr::progress, but with externally computed start and endpoints
# necessary to handle chunked transfers properly
# call init_storage_progress_bar before starting file transfer
# call clear_storage_progress_bar when file transfer complete

storage_progress_bar <- R6::R6Class("storage_progress_bar",

public=list(
    bar=NULL,
    direction=NULL,
    size=NULL,
    offset=NULL,

    initialize=function(size, direction)
    {
        self$direction <- direction
        self$size <- size
        self$offset <- 0
        self$bar <- utils::txtProgressBar(min=0, max=size, style=3)
    },

    update=function()
    {
        direction <- self$direction
        offset <- self$offset
        size <- self$size

        func <- function(down, up)
        {
            now <- offset + if(direction == "down") down[[2]] else up[[2]]
            utils::setTxtProgressBar(self$bar, now)
        }

        # hack b/c request function is not exported by httr
        req <- list(method=NULL, url=NULL, headers=NULL, fields=NULL,
                    options=list(noprogress=FALSE, progressfunction=func))
        structure(req, class="request")
    },

    finalize=function()
    {
        try(close(self$bar), silent=TRUE)
    }
))
