

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

rmd.templates <- function(){
  system.file("rmd", package = "adjustr")
}



#' Generates Pdf or Html document containing the analysis of a \strong{discrete} random variable
#'
#' @param output_file The name of the output file. If using NULL then the output filename will be based on filename for the input file. If a filename is provided, a path to the output file can also be provided. Note that the output_dir option allows for specifying the output file path as well, however, if also specifying the path, the directory must exist. If output_file is specified but does not have a file extension, an extension will be automatically added according to the output format. To avoid the automatic file extension, put the output_file value in \code{\link{I()}}, e.g., \code{I('my-output')}.
#' @param author The author to use in the template
#' @param path The pate to the csv file. note that the column that contains the random variable values must be named x
#' @param title The title to be used in the template
#' @param date The date to be used in the template
#' @param output_dir The output directory for the rendered output_file. This allows for a choice of an alternate directory to which the output file should be written (the default output directory of that of the input file). If a path is provided with a filename in output_file the directory specified here will take precedence. Please note that any directory path provided will create any necessary directories if they do not exist.
#' @param output_format The R Markdown output format to convert to. The option "all" will render all formats defined within the file. The option can be the name of a format (e.g. "html_document") and that will render the document to that single format. One can also use a vector of format names to render to multiple formats. Alternatively, you can pass an output format object (e.g. \code{html_document()}). If using NULL then the output format is the first one defined in the YAML frontmatter in the input file (this defaults to HTML if no format is specified there).
#' @param verbose An option to suppress printing of the pandoc command line.
#' @param ... Additional arguments to be passed to \code{\link{rmarkdown::render()}}
#'
#' @inherit rmarkdown::render return
#' @seealso \code{\link{rmarkdown::render()}} which this function wraps
#' @export
#'
#' @examples
#' \dontrun{
#' rmd.discrete("discrete",
#'                 author="Abdessabour MOUTIK",
#'                 path="~/Downloads/devoir20octobre2019/nombresinistre/nombresinistre18.csv",
#'                  output_format = "all", verbose=T)
#' }
rmd.discrete<- function(output_file, author, path, title="Ajustement d'une V.A discrète par adjustr", date=Sys.Date(), output_dir=getwd(), output_format, verbose=F, ...){
  #rmarkdown::render(paste0(rmd.templates,"/discrete.Rmd"), output_file = name, output_dir = "DLs", quiet=quiet , params = list(author=author, path=path), ...)
  rmd.render("discrete", output_file, author, path, title, date, output_dir, output_format, quiet=!verbose, ...)
}

#' Generates Pdf or Html document containing the analysis of a \strong{continuous} random variable
#'
#' @param output_file The name of the output file. If using NULL then the output filename will be based on filename for the input file. If a filename is provided, a path to the output file can also be provided. Note that the output_dir option allows for specifying the output file path as well, however, if also specifying the path, the directory must exist. If output_file is specified but does not have a file extension, an extension will be automatically added according to the output format. To avoid the automatic file extension, put the output_file value in \code{\link{I()}}, e.g., \code{I('my-output')}.
#' @param author The author to use in the template
#' @param path The pate to the csv file. note that the column that contains the random variable values must be named x
#' @param title The title to be used in the template
#' @param date The date to be used in the template
#' @param output_dir The output directory for the rendered output_file. This allows for a choice of an alternate directory to which the output file should be written (the default output directory of that of the input file). If a path is provided with a filename in output_file the directory specified here will take precedence. Please note that any directory path provided will create any necessary directories if they do not exist.
#' @param output_format The R Markdown output format to convert to. The option "all" will render all formats defined within the file. The option can be the name of a format (e.g. "html_document") and that will render the document to that single format. One can also use a vector of format names to render to multiple formats. Alternatively, you can pass an output format object (e.g. \code{html_document()}). If using NULL then the output format is the first one defined in the YAML frontmatter in the input file (this defaults to HTML if no format is specified there).
#' @param verbose An option to suppress printing of the pandoc command line.
#' @param ... Additional arguments to be passed to \code{\link{rmarkdown::render()}}
#'
#' @inherit rmarkdown::render return
#' @seealso \code{\link{rmarkdown::render()}} which this function wraps
#' @export
#'
#' @examples
#' \dontrun{
#' rmd.continuous("continuous",
#'                author="Abdessabour MOUTIK",
#'                path="~/Downloads/devoir20octobre2019/montantsisnitres/montantssinistre12.csv",
#'                output_format = "all",
#'                verbose=T)
#' }
rmd.continuous <- function(output_file, author, path, title="Ajustement d'une V.A continue par adjustr", date=Sys.Date(), output_dir=getwd(), output_format, verbose=F, ...){
  #rmarkdown::render(paste0(rmd.templates,"/discrete.Rmd"), output_file = name, output_dir = "DLs", quiet=quiet , params = list(author=author, path=path), ...)
  rmd.render("continuous", output_file, author, path, title, date, output_dir, output_format, quiet=!verbose, ...)
}

rmd.render<-function(type, output_file, author, path, title, date, output_dir, output_format, quiet, ...){
  rmarkdown::render(paste0(rmd.templates(),"/", type ,".Rmd"), output_format, output_file = output_file, output_dir = output_dir, quiet=quiet , params = list(author=author, path=path, date=date, title=title), ...)
}

#' @export
print.continuous <- function(x, ...){
  class(x) <- "continuous"

  cat("The parameter estimate", if(attr(x,"distribution")=="exponential") " " else "s (", p(x$estimate,sep=", "), if(attr(x,"distribution")=="exponential") "" else ")"
      , " estimated by `ML`",sep='')

  cat("\nAIC = ",paste0( x$aic,", p.value = "), x$p.value, sep='')
  cat("\nWe", if(x$accepthyp) "accept" else "reject","the null hypothesis")

  # print(p("\nObserved and fitted values for", attr(x,"distribution"),"distibution\n"))
  # print(p("with parameters estimated by `", p(attr(x, 'method'), sep=" "), "` \n\n"))
  #
  # print(data.frame(count=x$count, observed=x$observed, fitted=x$fitted), row.names=F, ...)
  # print(p("\nX-squared =", x$Xsquared, ", df = ",x$df, ", p.value = ", x$p.value))
  # print(p("\nWe", if(x$accepthyp) "accept" else "reject","the null hypothesis"))
  invisible(x)

}



#' Fitting with continuous distributions
#'
#' Calculates the best fitted distribution for the given random variable
#'
#' @param x Vector, the continuous random variable.
#' @param showplots Logical, determines whether to show plots.
#' @param short Logical, determines whether the output will contain the best fitted distribution or all of them.
#' @param color Named Vector, the colors to use in the plot.
#' @param plots.as.vars Logical, whether to add the plots to the returned list.
#' @param xlab a character string to label the x axis.
#'
#' @return A list containing only the best fitted distribution when \code{short=TRUE} otherwise it'll contain all the adjusted distributions.
#'  If the best fitted distribution exists, it can be accessed through \code{result$bestdistr}, and those who validated the Null
#'  Hypothesis are found at \code{result$acceptedDistr} ordered from the lowest AIC to the highest.
#' @examples
#' \dontrun{
#'   (fit <- getfitdistr(x))
#' }
#' @author Abdessabour MOUTIK
#' @export
getfitdistr <- function(x, xlab="Claim amounts", showplots=TRUE, short=TRUE, color=c(gamma = "steelblue", lognormal="firebrick",exponential= "green"), plots.as.vars=F){
  #library(latex2exp)
  #library(MASS)
  #require(ggplot2)
  aic <- function(l,p) -1*l + 2*p

  result <- list(distributions=c("gamma", "lognormal", "exponential"))

  result$acceptedDistr <-data.frame(distribution=character(0), aic=numeric(0), stringsAsFactors = FALSE)

  if(showplots || plots.as.vars){
    center <- theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.5, 0.2))

    mainplot <- ggplot(data.frame(x=seq(0, max(x),length.out= length(x)),var=x), aes(x=x)) +
      stat_ecdf(aes(var, colour="X"), color="black", show.legend=F) + labs(title=p("Empirical cumulative distribution function of ",xlab), x=xlab, y=p("F(",xlab,")")) +
      center

    if(showplots)  print(mainplot)
    if(plots.as.vars) result$plot <- mainplot
  }

  for (distribution in result$distributions) {

    fit <- fitdistr(x, distribution)
    result[[distribution]]$estimate <- fit$estimate
    sloi <- p("p", if(distribution=="exponential") "exp" else if(distribution=="lognormal") "lnorm" else distribution )

    ksstat <- do.call(ks.test, c(list(x, sloi), as.vector(result[[distribution]]$estimate)))

    result[[distribution]]$p.value <- ksstat$p.value
    result[[distribution]]$accepthyp <- ksstat$p.value > .05

    result[[distribution]]$aic <- aic(fit$loglik, if(distribution=="exponential") 1 else 2)

    if(result[[distribution]]$accepthyp){
      result$acceptedDistr[1,] <- c(distribution, result[[distribution]]$aic)
    }

    class(result[[distribution]]) <- c("continuous", "list")
    attr(result[[distribution]], "distribution") <- switch(distribution,
                                         "exponential"="exponential",
                                         "lognormal"="log-normal",
                                         "gamma"="gamma")
  }


  if(showplots || plots.as.vars) {
    combinedplot <- ggplot(data.frame(x=seq(0, max(x),length.out= length(x)),var=x), aes(x=x)) +
      stat_ecdf(aes(var, colour="ecdf")) +
      stat_function(aes(colour="gamma"), fun=pgamma , args=result$gamma$estimate) +
      stat_function(aes(colour="lognormal"), fun=plnorm, args=result$lognormal$estimate) +
      stat_function(aes(colour="exponential"), fun=pexp, args=result$exponential$estimate) +
      scale_colour_manual("the distribution function of:", values=c(ecdf="black",color), label=c(xlab, "Exponentiel", "Gamma", "Log-normal")) +
      scale_y_continuous(labels=scales::percent) + labs(title=p("Fitting of  the distribution function ", xlab),x=xlab, y=p("F(",xlab,")")) + center
    if(plots.as.vars)  result$combinedplot <- combinedplot
    if(showplots)  print(combinedplot)
  }

  #if(showplots && !detailed.curves) legend(x=11.5,y=0.35, legend=c("fn de r. de x", "fn de r. de la distribution gamma", "fn de r. de la distribution lognormal", "fn de r. de la distribution exponentiel"), lty=1:4, col = c("black", as.character(color)))

  if (length(result$acceptedDistr$distribution)==0) {print("All the distributions failed to confirm the null hypothesis");return(result)}

  result$acceptedDistr <- result$acceptedDistr[with(result$acceptedDistr,order(-as.numeric(aic), distribution)),]
  result$bestdistr <- result$acceptedDistr[1,"distribution"]
  if(short) {
    distribution=result$bestdistr;
    c(list(distribution=distribution), result[[distribution]][names(result[[distribution]]) != "accepthyp"])
  }  else result
}




#' Fitting with discrete distributions
#'
#' Calculates the best fitted distribution for the given random variable
#'
#' @param x Vector, the discrete random variable.
#' @param showplots Logical, determines whether to show plots.
#' @param short Logical, determines whether the output will contain the best fitted distribution or all of them.
#' @param plots.as.vars Logical, whether to add the plots to the returned list.
#' @param xlab a character string to separate the terms.
#'
#' @return A list containing only the best fitted distribution when \code{short=TRUE} otherwise it'll contain all the adjusted distributions.
#'  If the best fitted distribution exists, it can be accessed through \code{result$bestdistr}, and those who validated the Null
#'  Hypothesis are found at \code{result$acceptedDistr} ordered from the lowest AIC to the highest.
#' @examples
#' \dontrun{
#'   (fit <- getgoodfit(x))
#' }
#' @author Abdessabour MOUTIK
#' @export
getgoodfit <- function(x, showplots=F, short=TRUE, plots.as.vars=F, xlab="Number of claims"){
  #library(vcd)
  #require(ggplot2)
  result <- list(distributions=c("pois", "binom", "nbinom"))
  default.length <- length(table(x))-1
  moy = mean(x)
  v = var(x)

  center <- theme(plot.title = element_text(hjust = 0.5))

  result$acceptedDistr <-data.frame(distribution=character(0), Xsquared=numeric(0), stringsAsFactors = FALSE)

  if(showplots || plots.as.vars) {
    pp <- ggplot(data.frame(var=as.numeric(table(x)), l=0:(length(table(x))-1)),aes(x=l, y=var)) + geom_bar(stat="identity", colour = 'black', fill='#eeeeee') + labs(title=p("Distribution of ", xlab), x=xlab, y="Frequency") + center
    if(plots.as.vars) result$Xplot <- pp
    if(showplots) print(pp)
  }
  for (distribution in result$distributions) {
    if(distribution!="pois"){
      par <- list( size = getsize(moy, v, distribution) )
      if(par$size<0 || is.na(par$size)) {print(p("Error occured whilst estimating the size n = ", par$size, " of the ", if(distribution=="nbinom") "negative binomial" else "binomial", " distribution.")); par$size <- default.length}
      fit <- goodfit(x, distribution, par = par)
    } else {
      fit <- goodfit(x, distribution)
    }

    #if(showplots) {
    #   sloi <- TeX(p("Ajustement par une distrubition  ", if(distribution=="pois") "de Poisson" else if(distribution=="nbinom") "Binomiale négative" else "Binomiale","(", p(round(as.numeric(fit$par),digits=2), sep=", "),")"));#sapply(fit$par, function(x) round(x,digits=2))
    #   pp <- ggplot(data=data.frame( number=fit$count, fitted=sqrt(fit$fitted), observed=sqrt(fit$observed)), aes(x=number, y=fitted)) + geom_bar(aes(y=observed), stat="identity", colour="lightcyan") + geom_line(size=2,colour="firebrick")  + geom_errorbar(aes(ymin = observed, ymax = fitted), colour="#7aadd1", size = .85, width=0.25) + geom_point(size=4, colour="grey") + labs(title=sloi, x=xlab, y=TeX("$\\sqrt{Frequency}$")) + center
    #   if(plots.as.vars) result[[distribution]]$plot <- pp
    #   print(pp)
    #   rm(pp)
    # }
    fitted <- fit$fitted
    observed <- fit$observed

    l = length(observed)

    while(sum(fitted<=5)>l/10 && l>2){
      fitted <- sumlast(fitted, l)
      observed <- sumlast(observed, l)
      l <- l-1
    }

    distr <- do.call(p("d", distribution), c(list(x=0:(l-2)),list(lambda=fit$par$lambda)[distribution=="pois"], list(size=fit$par$size, prob=fit$par$prob)[distribution!="pois"]))
    prob <- c(distr, 1-sum(distr))

    df <- l-(if(distribution=="pois") 2 else 3)
    df <- if(df==0) 1 else df
    chisqstat <- chisq.test(observed, p=prob)

    Xsquared <- as.numeric(chisqstat$statistic)
    p.value <- as.numeric(1-pchisq(Xsquared, df))

    accepthyp <- p.value > .05 && Xsquared < as.numeric(qchisq(0.95,df))

    count <- c(0:(l-1))

    if(!is.na(accepthyp)){
      if(accepthyp) result$acceptedDistr[1,] <- c(distribution, Xsquared)
    }
    result[[distribution]] <- list(observed=fit$observed, count = fit$count, fitted=fit$fitted, prob = prob )

    result[[distribution]]$estimate <- fit$par
    result[[distribution]]$p.value <- p.value
    result[[distribution]]$Xsquared <- Xsquared
    result[[distribution]]$df <- df
    result[[distribution]]$accepthyp <- accepthyp
    result[[distribution]]$length <- l
    class(result[[distribution]]) <- c("discrete", "list")
    attr(result[[distribution]], "distribution") <- switch(distribution,
                                         "pois"="poisson",
                                         "binom"="binomial",
                                         "nbinom"="negative binomial")
    attr(result[[distribution]], 'method') <- fit$method
    attr(result[[distribution]], 'xlab') <- xlab
    if(showplots || plots.as.vars){
      plot <- autoplot(result[[distribution]])
      if(plots.as.vars) result[[distribution]]$plot = plot
      if(showplots) print(plot)
    }
    result[[distribution]]$observed = observed
    result[[distribution]]$count = count
    result[[distribution]]$fitted = fitted
  }

  if (length(result$acceptedDistr$distribution)==0) {print("All the distributions failed to confirm the null hypothesis");return(result)}

  result$acceptedDistr <- result$acceptedDistr[with(result$acceptedDistr, order(-as.numeric(Xsquared), distribution)),]
  result$bestdistr <- result$acceptedDistr[1,"distribution"]

  if(short) {
     result[[result$bestdistr]]#[names(result[[distribution]])!= "accepthyp"])
  }  else result
}



sumlast <- function(y,l=length(y)) {l<-l-2;c(y[1:l], sum(y[(l+1):(l+2)]))}


getsize <- function(moyenne, variance, distribution) {
  v = as.numeric(variance)
  m = as.numeric(moyenne)
  n <- NA
  if(distribution=="nbinom" && m<v){
    n <- (log((m/v)*m)) / log(1-(m/v))
  }else if(distribution=="binom" && m!=v){
    n <- round(m/(1-(v/m)))
  }
  n
}

# @importFrom ggplot2 autoplot
# @export autoplot

#autoplot <- autoplot

##' @export autoplot.discrete
##'
##' @method autoplot discrete
autoplot.discrete<- function(object,
      colour = c("black", "#B61A51"), fill = "#eeeeee", size = c(1.2, 4), width=0.9, ...){
  #require(ggplot2)
  # #class(object) <- "data.frame"
  # observed <- sqrt(object$observed)
  # fitted <- sqrt(object$fitted)
  #l <- length(object$observed)
  #size <- if (l>10)  0.75 + 5 * size / l else size
  #width <- if (l>10)  0.75 + 5 * width / l else width
#  data.frame(height = sqrt(object$observed), line= sqrt(object$fitted), width=width, x=object$count) %>% ggplot2::ggplot(aes(x=x, y=line, xmin=x-(width/2), xmax=x+(width/2), ymin=line-height,ymax=line)) + ggplot2::geom_rect( colour=colour[1], fill=fill) + ggplot2::geom_line( size = size[1], colour=colour[2])+ ggplot2::geom_point( size = size[2], colour=colour[2]) + ggplot2::geom_hline(yintercept = 0) + ggplot2::xlab(attr(object,"xlab")) +
 #   ggplot2::ylab(latex2exp::TeX("$\\sqrt{\\textit{Frequency}}$"))
  ggplot(data.frame(height = sqrt(object$observed), line= sqrt(object$fitted), width=width, x=object$count), aes(x=x, y=line, xmin=x-(width/2), xmax=x+(width/2), ymin=line-height,ymax=line)) + geom_rect( colour=colour[1], fill=fill) + geom_line( size = size[1], colour=colour[2])+ geom_point( size = size[2], colour=colour[2]) + geom_hline(yintercept = 0) + xlab(attr(object,"xlab")) +
    ylab(latex2exp::TeX("$\\sqrt{\\textit{Frequency}}$"))
}


print.discrete <- function(x, ...){
  class(x) <- "discrete"

  cat("\nObserved and fitted values for", attr(x,"distribution"),"distibution\n")
  cat("with parameter", if(attr(x,"distribution")=="poisson") " " else "s (", p(x$estimate,sep=", "), if(attr(x,"distribution")=="poisson") "" else ")", " estimated by ",p("`", p(attr(x, 'method'), sep=" "),"`"), "\n\n", sep='')

  print(data.frame(count=x$count, observed=x$observed, fitted=x$fitted,prob=x$prob), row.names=F, ...)
  cat("\nX-squared = ", x$Xsquared, ", df = ",x$df, ", p.value = ", x$p.value, sep='')
  cat("\nWe", if(x$accepthyp) "accept" else "reject","the null hypothesis")

  # print(p("\nObserved and fitted values for", attr(x,"distribution"),"distibution\n"))
  # print(p("with parameters estimated by `", p(attr(x, 'method'), sep=" "), "` \n\n"))
  #
  # print(data.frame(count=x$count, observed=x$observed, fitted=x$fitted), row.names=F, ...)
  # print(p("\nX-squared =", x$Xsquared, ", df = ",x$df, ", p.value = ", x$p.value))
  # print(p("\nWe", if(x$accepthyp) "accept" else "reject","the null hypothesis"))
  invisible(x)
  #X-squared = 33.884, df = 6, p-value = 7.083e-06

#
#   inferlaw.method <- function(x) x<-x$estimate;if(length(x)==1) c("poisson","ML") else  c(if(x[1]>1) "nbinomial" else "binomial","ML","with size fixed")
#
#   law.mthd <- inferlaw.method(object)
#
#   object$method =law.mthd[2:3][is.null(law.mthd[2:3])]
#   object$type = law.mthd[1]
#   object$par <- object$estimate
#   class(object) <- "goodfit"
#
#   #print(names(object))
#   object
}



p <- function(..., sep='') {
  paste(..., sep=sep, collapse=sep)
}


# register_s3_method <- function(pkg, generic, class, fun = NULL) {
#   stopifnot(is.character(pkg), length(pkg) == 1)
#   stopifnot(is.character(generic), length(generic) == 1)
#   stopifnot(is.character(class), length(class) == 1)
#
#   if (is.null(fun)) {
#     fun <- get(paste0(generic, ".", class), envir = parent.frame())
#   } else {
#     stopifnot(is.function(fun))
#   }
#
#   if (pkg %in% loadedNamespaces()) {
#     registerS3method(generic, class, fun, envir = asNamespace(pkg))
#   }
#
#   # Always register hook in case package is later unloaded & reloaded
#   setHook(
#     packageEvent(pkg, "onLoad"),
#     function(...) {
#       registerS3method(generic, class, fun, envir = asNamespace(pkg))
#     }
#   )
# }
#
#
# .onLoad <- function(...) {
#   register_s3_method("knitr", "knit_print", "discrete")
# }


# esmtimate= result[[distribution]]$estimate, Xsquared=result[[distribution]]$Xsquared, observed=result[[distribution]]$observed, count=result[[distribution]]$count,df=result[[distribution]]$df,fitted=result[[distribution]]$fitted, p.value=result[[distribution]]$p.value)

#if(detailed.curves) {plot(ecdf(x), main = );legend(x=11.5,y=0.35, legend=c("fn de r. de x", p("fn de r. de la distribution ", distribution) ), lty=1:2, col = c("black", color[[distribution]]))}

#pp<-ggplot(df, aes(x=x, y=y)) + stat_ecdf(aes(z), colour='black', linetype=2)
#ggdistribution(pgamma, seq(0, max(x),length.out= length(x)), shape=result$gamma$estimate[[1]],rate=result$gamma$estimate[[2]], colour = 'red', p=pp)
#pp
#pp <- ggplot( data=data.frame(var=c(distr, x) , col = c( )), aes(var, colour=col)) + stat_ecdf(size=1) + scale_colour_manual(values = c("#000000", "firebrick"))
#print(pp)
