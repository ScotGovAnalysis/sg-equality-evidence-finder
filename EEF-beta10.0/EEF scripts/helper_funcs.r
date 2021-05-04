###################################################################################
#                                                                                 #
# HELPER FUNCTIONS - TEMPORARY FIXES                                              #
#                                                                                 #
###################################################################################
#                                                                                 #
# Author: Jay Ware                                                                #
# Created: 01/08/2018                                                             #
# Last updated: 14/01/2021                                                        #
#                                                                                 #
# Purpose: Use newer version of some shinyjs functions                            #
#          THIS LIKELY IS NO LONGER NEEDED, AND DEFINITELY NOT NEEDED WHEN WE     #
#          MOVE TO R 3.6                                                          #
#                                                                                 #
###################################################################################

#shinyjs bugfix - shinyjs version 1.0 and earlier contains a bug when using modules so overwrite with the shinyjs 1.01 version - temp fix 
#TO DO - this can be deleted once we migrate to SCOTS R 3.6
if(packageVersion("shinyjs") <= "1.0"){
  show <- function (id = NULL, anim = FALSE, animType = "slide", time = 0.5, 
                    selector = NULL) 
  {
    fxn <- "show"
    params <- list(id = id, anim = anim, animType = animType, 
                   time = time, selector = selector)
    jsFuncHelper(fxn, params)
  }
  
  hide <- function (id = NULL, anim = FALSE, animType = "slide", time = 0.5, 
                    selector = NULL) 
  {
    fxn <- "hide"
    params <- list(id = id, anim = anim, animType = animType, 
                   time = time, selector = selector)
    jsFuncHelper(fxn, params)
  }
  
  removeClass <- function(id = NULL, class = NULL, selector = NULL) { 
    fxn <- "removeClass" 
    params <- list(id = id, class = class, selector = selector) 
    jsFuncHelper(fxn, params) 
  } 
  
  jsFuncHelper <- function(fxn, params) { 
    # get the Shiny session 
    session <- getSession() 
    
    
    fxn <- paste0("shinyjs-", fxn) 
    
    
    # respect Shiny modules/namespaces 
    if (inherits(session , "session_proxy")) { 
      if ("id" %in% names(params) && !is.null(params[['id']])) { 
        params[['id']] <- session$ns(params[['id']]) 
      } 
    } 
    
    
    # call the javascript function 
    session$sendCustomMessage( 
      type = fxn, 
      message = params) 
    
    
    invisible(NULL) 
  } 
  errMsg <- function(x) { 
    stop(sprintf("shinyjs: %s", x), call. = FALSE) 
  } 
  
  
  # get the shiny session object 
  getSession <- function() { 
    session <- shiny::getDefaultReactiveDomain() 
    
    
    if (is.null(session)) { 
      errMsg(paste( 
        "could not find the Shiny session object. This usually happens when a", 
        "shinyjs function is called from a context that wasn't set up by a Shiny session." 
      )) 
    } 
    
    
    session 
  } 
}
