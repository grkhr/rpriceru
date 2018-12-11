#' Get stats
#'
#' Function to get stats
#' @param login Your account e-mail
#' @param password Your account password
#' @param token Your API token, you can get it from technical support
#' @param date_start Start date
#' @param date_end End date
#' @param shop_id Your shop id, see \code{\link{https://biz.price.ru/shops}}. See "Magazin ID: "
#' @param id Your price-list id, see \code{\link{https://biz.price.ru/shops}}. See "ID: ..... Price-list activen"
#' @export
#' @importFrom httr POST
#' @importFrom httr GET
#' @importFrom httr stop_for_status
#' @importFrom httr content
#' @importFrom httr add_headers
#' @importFrom rjson toJSON
#' @importFrom rjson fromJSON
#' @examples
#' PriceRuStatsAPI()


PriceRuStatsAPI <- function(login = NULL, password = NULL, token = NULL, date_start = NULL, date_end = NULL, shop_id = NULL, id = NULL)
{
  proc_start <- Sys.time()
  packageStartupMessage("Processing", appendLF = F)
  date_start = as.character(date_start)
  date_end = as.character(date_end)
body_list <- list(method = "Login", 
                  params = list(
                    login =  login,
                    password = password
                    ),
                  token = token
              )   
answer <- POST("https://smart.begun.ru/api/", body = toJSON(body_list))
dataRaw <- content(answer, as = "parsed", encoding = "UTF-8", "application/json")

session_id = dataRaw$data$session_id

body_list <- list(method = "PriceList.GetStats", 
                  params = list(
                  #  shop_id = shop_id,
                    id = id,
                    report_type = "daily",
                    date_start = date_start,
                    date_end = date_end,
                    is_limit_off = TRUE
                  ),
                  session_id = session_id,
                  token = token)   
answer <- POST("https://smart.begun.ru/api/", body = toJSON(body_list))
dataRaw <- content(answer, as = "parsed", encoding = "UTF-8", "application/json")
dataRaw <- dataRaw$data$list
packageStartupMessage(".", appendLF = F)

result <- data.frame()
if (length(dataRaw)>0)
{
column_names <- unlist(lapply(c(names(dataRaw[[1]])), 
                              function(x) return(x)))

rows <- lapply(dataRaw, function(x) return(x))
for (rows_i in 1:length(rows)) {
  result <- rbind(result, unlist(rows[[rows_i]]),stringsAsFactors = F)
}
colnames(result) <- column_names
}

body_list <- list(method ="Ads.Campaign.GetList", 
                  params = list(
                     shop_id = shop_id,
                     filters = list(
                     cost_type = "remarketing"
                     )
                    #id = id,
                    #report_type = "daily",
                    #date_start = date_start,
                    #date_end = date_end,
                    #is_limit_off = TRUE
                  ),
                  session_id = session_id,
                  token = token)   
answer <- POST("https://smart.begun.ru/api/", body = toJSON(body_list))
dataRaw <- content(answer, as = "parsed", encoding = "UTF-8", "application/json")
dataRaw <- dataRaw$data$list
packageStartupMessage(".", appendLF = F)

if (length(dataRaw)!=0)
{
 
  res <- list()
  for (i in 1:length(dataRaw))
    {
       res[[i]] <- dataRaw[[i]]$campaign_id
  }
  resrem <- data.frame()
  for (i in 1:length(res))
  {
    body_list <- list(method ="Ads.Campaign.GetStats", 
                      params = list(
                        campaign_id = res[[i]],
                        report_type = "ShowsDayRemarketing",
                       # shop_id = shop_id,
                        #id = id,
                        #report_type = "daily",
                        date_start = date_start,
                        date_end = date_end,
                      # only_filters = TRUE,
                     #  mapping = "brief",
                        is_limit_off = TRUE
                      ),
                      session_id = session_id,
                      token = token)   
    answer <- POST("https://smart.begun.ru/api/", body = toJSON(body_list))
    dataRaw <- content(answer, as = "parsed", encoding = "UTF-8", "application/json")
    dataRaw <- dataRaw$data$list
    if (length(dataRaw) != 0)
    {
    column_names <- unlist(lapply(c(names(dataRaw[[1]])), 
                                  function(x) return(x)))
    
    rows <- lapply(dataRaw, function(x) return(x))
    for (rows_i in 1:length(rows)) {
      resrem <- rbind(resrem, unlist(rows[[rows_i]]),stringsAsFactors = F)
         }
    }
    packageStartupMessage(".", appendLF = F)
  }
  if (length(resrem)>0)
  {
  column_names <- gsub("ts","date",column_names) 
 colnames(resrem) <- column_names
 resrem[["date"]] <- as.Date(as.POSIXct(as.numeric(resrem[["date"]]), origin="1970-01-01"))
 resrem$cpm <- NULL
 result <- rbind.fill(result,resrem)
  }
 
}
packageStartupMessage(appendLF = T)
packageStartupMessage("Processed ",length(result$shows)," rows", appendLF = T)
total_work_time <- round(difftime(Sys.time(), proc_start , units ="secs"),0)
packageStartupMessage(paste0("Total time: ",total_work_time, " sec."))
return(result)
}








