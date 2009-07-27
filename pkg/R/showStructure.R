"showStructure" <-
function(x, maxlen=20, describeAttributes=TRUE, short=NULL,
         prefix="", attri=FALSE,
         ...){
  # Describe the structure of object x, recursively.
  # maxlen: integer; if x is a list of length longer than maxlen, only list the
  #         names of the components.  May be a vector, Kth element used
  #         for Kth level of recursion
  # describeAttributes: if FALSE, then only give names of attributes.
  # short:  logical, this argument may be used by methods
  #         (currently used by bdFrame and bdVector methods in S-PLUS)
  # ... :   passed to methods
  # prefix: for internal use in recursive calls, for indenting levels
  # attri:  for intended use in recursive calls; if TRUE then attributes
  #         are being printed, use & instead of $ or @

  # This function has not been tested with all kinds of objects,
  # 

  if(is.environment(x)){
    cat(prefix,"environment",
	if(class(x)[1] != "environment") paste("of class",class(x)[1]),
	"\n")
    return(invisible(NULL))
  }

  # Determine attributes to be shown
  ax <- attributes(x)
  ax$dim <- NULL
  ax$class <- NULL
  if(is.list(x) )
    ax$names <- NULL

  # Determine attributes to be shown separately
  #  -- none if !describeAttributes,
  #  -- otherwise everything but names and dimnames
  if(describeAttributes){
    ax1 <- ax[match(c("names","dimnames"), names(ax), nomatch=0)]
    if(!is.recursive(x)) ax1$names <- NULL
    ax2 <- ax
    ax2$names <- NULL
    ax2$dimnames <- NULL
  }
  else {
    ax1 <- ax
    ax2 <- NULL
  }

  # Is the object an S4 class (not including classes like "matrix")
  S4 <- isS4(x)

  # In R, attributes(x) includes S4 slots
  if(S4)
    ax2 <- ax2[!is.element(names(ax2), slotNames(x))]

  # In S+, unclass turns an S4 object into a list; not in R.
  ux <- unclass(x)
  len <- length(ux)
  if(S4)
    len <- length(slotNames(x))

  # Loop over list components or slots
  if(!attri){
    if(mode(x) == "numeric" && is.null(dim(x)) && len == 1)
      cat(prefix,"scalar",sep="")
    else if(is.null(x))
      cat(prefix,"NULL")
    else {
      cat(prefix, mode(x),"[",sep="")
      if(is.null(dim(x)))
	cat(" length", len)
      else
	cat(dim(x),sep=",")
      cat("]")
    } # Don't print "\n" yet.

    # List class after name of object.  Note that cat() handles
    # vectors by collapsing into a single vector.
    if(S4) {
      cat("  S4 class:", class(x), "\n")
    } else {
      if(length(attr(x, "class")))
        cat("  S3 class:", oldClass(x), "\n")
      else
        cat("  class:", class(x), "\n")
    } 
  }

  # Attributes
  if(length(ax1))
    cat(prefix, "attributes:", names(ax1), "\n")
  if(length(ax2))
    Recall(ax2, maxlen = if(length(maxlen) > 1) maxlen[-1] else maxlen,
	   prefix = prefix, describeAttributes=TRUE, attri=TRUE,
           short=short, ...)

  nn <- slotNames(x)
  slotted <- length(nn) && !is.element(class(x)[1], c("named", "structure"))
  if(!slotted){
    nn <- names(x)
    if(is.null(nn))
      nn <- 1:len
  }
  if(S4 || (is.list(ux) && len)){
    if(len <= maxlen){
      nn[nn==""] <- (1:len)[nn==""]
      nn <- format(nn, justify="left")
      for(i in 1:len){
	cat(prefix,
            ifelse1(attri, " &", S4 || slotted, " @", " $"),
	    nn[i], " ", sep="")
	Recall(ifelse1(S4, slot(x,slotNames(x)[i]), ux[[i]]),
	       maxlen = if(length(maxlen) > 1) maxlen[-1] else maxlen,
	       prefix = paste(prefix,"  ",sep=""),
	       describeAttributes=describeAttributes, short=short, ...)
      }
    } else {
      cat(prefix, "length > maxlen,",
          ifelse1(!slotted && is.null(names(x)), "No names", c("names= ",nn)),
          "\n")
    }
  }
  invisible(NULL)
}
