usps_get_page <- function(street, unit = NULL, city = NULL, state = NULL, zip = NULL) {
  my_url <- "https://tools.usps.com/tools/app/ziplookup/zipByAddress"
  
  my_ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36"

  form <- list(
    address1 = street,
    address2 = unit,
    city = city,
    state = state,
    zip = zip
  )

  resp <- httr::POST(
    my_url,
    httr::user_agent(my_ua),
    body = form,
    encode = "form"
  )

  html <- httr::content(resp)

  html
}



usps_get_result <- function(html, n) {
  result <- c(
    html$addressList[[n]]$addressLine1,
    html$addressList[[n]]$city,
    html$addressList[[n]]$state,
    html$addressList[[n]]$zip5,
    html$addressList[[n]]$zip4,
    html$addressList[[n]]$countyName,
    html$addressList[[n]]$dpvConfirmation
  )

  result
}



usps_df_all <- function(df) {
  vars_addr <- c("street", "unit", "city", "state", "zip")

  if (!is.data.frame(df)) {
    stop("Input is not a dataframe")
  } else if (!all(vars_addr %in% colnames(df))) {
    stop("Expected column names not found")
  }

  # If one extra column is present in `df`, it becomes the row ID in `df_usps`
  if ((length(colnames(df)) - length(vars_addr)) == 1) {
    row_id <- colnames(df)[which(!colnames(df) %in% vars_addr)]
    df_usps <- data.frame(matrix(nrow = 0, ncol = 10))
    colnames(df_usps) <- c(
      row_id, "n_row_src", "n_result", "street", "city",
      "state", "zip5", "zip4", "county", "DPV"
    )
  } else {
    df_usps <- data.frame(matrix(nrow = 0, ncol = 9))
    colnames(df_usps) <- c(
      "n_row_src", "n_result", "street", "city",
      "state", "zip5", "zip4", "county", "DPV"
    )
  }
  
  j <- 1
  k <- -1

  withProgress(message = "Getting addresses", {
    for (i in 1:nrow(df)) {
      if (exists("row_id")) id <- df[i, row_id]
      
      if (j > 1) {
        k <- k + j - 1
      }
      
      html <- try(
        usps_get_page(
          street = df$street[i],
          unit = df$unit[i],
          city = df$city[i],
          state = df$state[i],
          zip = df$zip[i]
        )
      )
      
      if ("try-error" %in% class(html) | length(html$addressList) == 0) {
        n_results <- 1
      } else {
        n_results <- length(html$addressList)
      }
      
      for (j in 1:n_results) {
        r <- i + j + k
        
        df_usps[r,] <- ""
        if (exists("row_id")) df_usps[r, row_id] <- id
        df_usps$n_row_src[r] <- i
        df_usps$n_result[r] <- j
        
        if ("try-error" %in% class(html) | length(html$addressList) == 0) {
          df_usps$street[r] <- "No result or unexpected/missing input"
          next
        }
        
        result <- usps_get_result(html, j)
        
        df_usps$street[r] <- result[1]
        df_usps$city[r] <- result[2]
        df_usps$state[r] <- result[3]
        df_usps$zip5[r] <- result[4]
        df_usps$zip4[r] <- result[5]
        df_usps$county[r] <- result[6]
        df_usps$DPV[r] <- result[7]
      }
      
      incProgress(1 / nrow(df))
    }
  })

  df_usps
}

