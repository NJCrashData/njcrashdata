addCredits <- function(username=getTAMUun(), password=getTAMUpw(), loginurl="https://geoservices.tamu.edu/Login/default.aspx", activityurl="https://geoservices.tamu.edu/UserServices/Payments/Partners/default.aspx", balance_threshold=2500) {
  ## adapted from @daroczig answer on stackoverflow
  ## http://stackoverflow.com/a/15897960/1492421

  require(RCurl)

  ## Check the balance. If it is above balance_threshold (generally 2,500), do not proceed
  creditBalance_before <- get_available_credits_tamu()
  if (creditBalance_before > balance_threshold) {
    message("Credit balance is ", formnumb(creditBalance_before), "\nCannot add credits until balance drops below ", formnumb(balance_threshold) )
  }

  ## Set curl options:
  curl <- getCurlHandle()
  curlSetOpt(cookiejar='cookies.txt'
          , followlocation=TRUE
          , autoreferer=TRUE
          , curl=curl
          , agent=getUserAgent()
          )

  ## Load the page for the first time to capture VIEWSTATE and other hidden params:
  html_prelogin <- getURL(loginurl, curl = curl)

  ## Set the manually required parameters
  params_user <- list(
      'ctl00$ContentPlaceHolderSectionBody$hiddenBodyPlaceholder'  = '',
      'ctl00$ContentPlaceHolderSectionBody$txtUserName'  = username,
      'ctl00$ContentPlaceHolderSectionBody$txtPassword'  = password
  )
  ## Add in the server parameters
  params <- addOtherInputParamsFromHTML(html=html_prelogin, params_user=params_user, showWarnings=TRUE)

  ## Log in
  html_loggedin <- postForm(loginurl, .params = params, curl = curl)
  if (!grepl('Logout', html_loggedin))
    warning ("greping for \"Logout\" failed -- may not be logged in correctly")

  ## Add Credits. First Solve a math puzzle
  pat.math <- "[[0-9a-zA-Z+/=_\\-\\$]+Math.*?"
  html_premath <- getURL(activityurl, curl=curl)
  math_IDs <- extractAttrValueFromHTML(html_premath, pat=pat.math)

  ## sloppy extraction of the two math values
  math_values_to_add <- 
    sapply(math_IDs[1:2], function(math_id) {
      val <- splitOnFirst(html_premath, paste0("id=\"", math_id, "\\\">"))[[c(1, 2)]]
      extractAllMatches("^\\d+", val)
    })

  ## Parameters for math form
  params.math <- list(
    `ctl00$ctl00$ctl00$ctl00$ctl00$ContentPlaceHolderBody$ContentPlaceHolderSectionMainBody$ContentPlaceHolderSectionMainBody$ContentPlaceHolderSectionMainBody$ContentPlaceHolderSectionBody$txtMathAnswer` = sum(as.numeric(math_values_to_add))
    ,
    `ctl00$ctl00$ctl00$ctl00$ctl00$ContentPlaceHolderBody$ContentPlaceHolderSectionMainBody$ContentPlaceHolderSectionMainBody$ContentPlaceHolderSectionMainBody$ContentPlaceHolderSectionBody$btnAddCredits` = 'Add Credits'
    )
  ## Add in validation parameters
  params.math <- addOtherInputParamsFromHTML(html_premath, param=params.math, justValidators=TRUE)

  ## Post the form to add the credits
  html_creditsadded <- postForm(activityurl, .params = params.math, curl = curl)

  ## Check the balance again to confirm the credits were added succesfully
  creditBalance_after <- get_available_credits_tamu()

  if (creditBalance_before >= creditBalance_after) {
    warning("Internal Error: Credit balance did not increase after running addCredits()\nMORE INFO: creditBalance_before = ", creditBalance_before , ";  creditBalance_after = ", creditBalance_after, call.=FALSE)
  } else if (verbose) {
    message("Credit balance has been increased to ", formnumb(creditBalance_after))
  }

  ## cleanup
  rm(curl)
  gcQuietly(verbose=FALSE)
  return(invisible(creditBalance_after))
}
