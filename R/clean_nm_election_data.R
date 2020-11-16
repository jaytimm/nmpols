
#' A description.
#'
#' A more detailed escription.
#' @name clean_nm_election_data
#' @param dir A directory
#' @return A dataframe
#' @import data.table readxl
#'
#' @export
#' @rdname clean_nm_election_data


# dir <- '/home/jtimm/jt_work/GitHub/packages/nmpols/data-raw/election-returns/'

clean_nm_election_data <- function(dir,
                                   is_precinct = TRUE) {


  outs <- list()
  setwd(dir)
  gfiles <- list.files(path = dir,
                       pattern = "xlsx",
                       recursive = TRUE)

  # tmp <- tempfile()
  # filename <- paste0(tmp,'.xlsx')
  # httr::GET(x, write_disk(filename))

  for (i in 1:length(gfiles)) {

    x <- paste0(dir, gfiles[i])
    sheets <- readxl::excel_sheets(x)

    outs[[i]] <- data.table::rbindlist(

      lapply(sheets,

             function(y) {

               dets <- suppressMessages(
                 readxl::read_excel(x,
                                    sheet = y,
                                    col_names = F)  )

               zz <- readxl::read_excel(
                 x,
                 sheet = y,
                 col_names = TRUE,
                 skip = ifelse(grepl('State', x), 6, 5))

               df1 <- zz[, -1]
               colnames(df1)[1] <- 'geo'
               data.table::setDT(df1)
               df2 <- reshape2::melt(df1,
                                     id.vars = c(1),
                                     measured.vars = c(2:ncol(df1)))

               df2$variable <- as.character(df2$variable)
               df2$election <- dets[[7,1]]
               df2$year <- gsub(' .*$', '', dets[[1,1]])

               ## if (is_precinct) {}
               df2$sheet <- y ##:
               df2
               })
      ) }

  ## no party information -- when to add -- ? not here --
  outs1 <- data.table::rbindlist(outs)
  outs1 <- subset(outs1, geo != 'TOTALS')
  outs1$value <- suppressWarnings(as.integer(outs1$value))
  outs1$value[is.na(outs1$value)] <- 0
  outs1[ , per := round(value / sum(value) * 100, 1) ,
         by = list(geo, election, year, sheet)]

  outs1 <- outs1[, c(5,4,1,2,3,7,6), with = FALSE]

  colnames(outs1) <- c("Year",
                       "RaceName",
                       #"PartyCode",
                       "AreaNum",
                       "CandidateName",
                       "CandidateVotes",
                       "CandidatePercentage",
                       'sheet')

  if(is_precinct) {

    outs1$AreaNum <-  gsub(' - .*$', '', outs1$AreaNum) # fix Hidalgo
    outs1$AreaNum <- paste0(outs1$sheet, ' County Precinct ',
                          as.numeric(gsub('^.* ', '', outs1$AreaNum)))
    outs1 <- outs1[, -7]

  }

  return(outs1)

}
