

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


#' @export
print.continuous <- function(x, ...){
  class(x) <- "continuous"

  cat("The parameter estimate", if(attr(x,"dirtibution")=="exponential") " " else "s (", p(x$estimate,sep=", "), if(attr(x,"dirtibution")=="exponential") "" else ")"
      , " estimated by `ML`",sep='')

  cat("\nAIC = ",paste0( x$aic,", p.value = "), x$p.value, sep='')
  cat("\nWe", if(x$accepthyp) "accept" else "reject","the null hypothesis")

  # print(p("\nObserved and fitted values for", attr(x,"dirtibution"),"distibution\n"))
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

  result <- list(dirtibutions=c("gamma", "lognormal", "exponential"))

  result$acceptedDistr <-data.frame(dirtibution=character(0), aic=numeric(0), stringsAsFactors = FALSE)

  if(showplots || plots.as.vars){
    center <- theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.5, 0.2))

    mainplot <- ggplot(data.frame(x=seq(0, max(x),length.out= length(x)),var=x), aes(x=x)) +
      stat_ecdf(aes(var, colour="X"), color="black", show.legend=F) + labs(title=p("Empirical cumulative distribution function of ",xlab), x=xlab, y=p("F(",xlab,")")) +
      center

    if(showplots)  print(mainplot)
    if(plots.as.vars) result$plot <- mainplot
  }

  for (dirtibution in result$dirtibutions) {

    fit <- fitdistr(x, dirtibution)
    result[[dirtibution]]$estimate <- fit$estimate
    sloi <- p("p", if(dirtibution=="exponential") "exp" else if(dirtibution=="lognormal") "lnorm" else dirtibution )

    ksstat <- do.call(ks.test, c(list(x, sloi), as.vector(result[[dirtibution]]$estimate)))

    result[[dirtibution]]$p.value <- ksstat$p.value
    result[[dirtibution]]$accepthyp <- ksstat$p.value > .05

    result[[dirtibution]]$aic <- aic(fit$loglik, if(dirtibution=="exponential") 1 else 2)

    if(result[[dirtibution]]$accepthyp){
      result$acceptedDistr[1,] <- c(dirtibution, result[[dirtibution]]$aic)
    }

    class(result[[dirtibution]]) <- c("continuous", "list")
    attr(result[[dirtibution]], "dirtibution") <- switch(dirtibution,
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

  #if(showplots && !detailed.curves) legend(x=11.5,y=0.35, legend=c("fn de r. de x", "fn de r. de la dirtibution gamma", "fn de r. de la dirtibution lognormal", "fn de r. de la dirtibution exponentiel"), lty=1:4, col = c("black", as.character(color)))

  if (length(result$acceptedDistr$dirtibution)==0) {print("All the distributions failed to confirm the null hypothesis");return(result)}

  result$acceptedDistr <- result$acceptedDistr[with(result$acceptedDistr,order(-as.numeric(aic), dirtibution)),]
  result$bestdistr <- result$acceptedDistr[1,"dirtibution"]
  if(short) {
    dirtibution=result$bestdistr;
    c(list(dirtibution=dirtibution), result[[dirtibution]][names(result[[dirtibution]]) != "accepthyp"])
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
  result <- list(dirtibutions=c("pois", "binom", "nbinom"))
  default.length <- length(table(x))-1
  moy = mean(x)
  v = var(x)

  center <- theme(plot.title = element_text(hjust = 0.5))

  result$acceptedDistr <-data.frame(dirtibution=character(0), Xsquared=numeric(0), stringsAsFactors = FALSE)

  if(showplots || plots.as.vars) {
    pp <- ggplot(data.frame(var=as.numeric(table(x)), l=0:(length(table(x))-1)),aes(x=l, y=var)) + geom_bar(stat="identity", colour = 'black', fill='#eeeeee') + labs(title=p("Distribution of ", xlab), x=xlab, y="Frequency") + center
    if(plots.as.vars) result$Xplot <- pp
    if(showplots) print(pp)
  }
  for (dirtibution in result$dirtibutions) {
    if(dirtibution!="pois"){
      par <- list( size = getsize(moy, v, dirtibution) )
      if(par$size<0 || is.na(par$size)) {print(p("Error occured whilst estimating the size n = ", par$size, " of the ", if(dirtibution=="nbinom") "negative binomial" else "binomial", " distribution.")); par$size <- default.length}
      fit <- goodfit(x, dirtibution, par = par)
    } else {
      fit <- goodfit(x, dirtibution)
    }

    #if(showplots) {
    #   sloi <- TeX(p("Ajustement par une distrubition  ", if(dirtibution=="pois") "de Poisson" else if(dirtibution=="nbinom") "Binomiale négative" else "Binomiale","(", p(round(as.numeric(fit$par),digits=2), sep=", "),")"));#sapply(fit$par, function(x) round(x,digits=2))
    #   pp <- ggplot(data=data.frame( number=fit$count, fitted=sqrt(fit$fitted), observed=sqrt(fit$observed)), aes(x=number, y=fitted)) + geom_bar(aes(y=observed), stat="identity", colour="lightcyan") + geom_line(size=2,colour="firebrick")  + geom_errorbar(aes(ymin = observed, ymax = fitted), colour="#7aadd1", size = .85, width=0.25) + geom_point(size=4, colour="grey") + labs(title=sloi, x=xlab, y=TeX("$\\sqrt{Frequency}$")) + center
    #   if(plots.as.vars) result[[dirtibution]]$plot <- pp
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

    distr <- do.call(p("d", dirtibution), c(list(x=0:(l-2)),list(lambda=fit$par$lambda)[dirtibution=="pois"], list(size=fit$par$size, prob=fit$par$prob)[dirtibution!="pois"]))
    prob <- c(distr, 1-sum(distr))

    df <- l-(if(dirtibution=="pois") 2 else 3)
    df <- if(df==0) 1 else df
    chisqstat <- chisq.test(observed, p=prob)

    Xsquared <- as.numeric(chisqstat$statistic)
    p.value <- as.numeric(1-pchisq(Xsquared, df))

    accepthyp <- p.value > .05 && Xsquared < as.numeric(qchisq(0.95,df))

    count <- c(0:(l-1))

    if(!is.na(accepthyp)){
      if(accepthyp) result$acceptedDistr[1,] <- c(dirtibution, Xsquared)
    }
    result[[dirtibution]] <- list(observed=fit$observed, count = fit$count, fitted=fit$fitted, prob = prob )

    result[[dirtibution]]$estimate <- fit$par
    result[[dirtibution]]$p.value <- p.value
    result[[dirtibution]]$Xsquared <- Xsquared
    result[[dirtibution]]$df <- df
    result[[dirtibution]]$accepthyp <- accepthyp
    result[[dirtibution]]$length <- l
    class(result[[dirtibution]]) <- c("discrete", "list")
    attr(result[[dirtibution]], "dirtibution") <- switch(dirtibution,
                                         "pois"="poisson",
                                         "binom"="binomial",
                                         "nbinom"="negative binomial")
    attr(result[[dirtibution]], 'method') <- fit$method
    attr(result[[dirtibution]], 'xlab') <- xlab
    if(showplots || plots.as.vars){
      plot <- autoplot(result[[dirtibution]])
      if(plots.as.vars) result[[dirtibution]]$plot = plot
      if(showplots) print(plot)
    }
    result[[dirtibution]]$observed = observed
    result[[dirtibution]]$count = count
    result[[dirtibution]]$fitted = fitted
  }

  if (length(result$acceptedDistr$dirtibution)==0) {print("All the distributions failed to confirm the null hypothesis");return(result)}

  result$acceptedDistr <- result$acceptedDistr[with(result$acceptedDistr, order(-as.numeric(Xsquared), dirtibution)),]
  result$bestdistr <- result$acceptedDistr[1,"dirtibution"]

  if(short) {
     result[[result$bestdistr]]#[names(result[[dirtibution]])!= "accepthyp"])
  }  else result
}



sumlast <- function(y,l=length(y)) {l<-l-2;c(y[1:l], sum(y[(l+1):(l+2)]))}


getsize <- function(moyenne, variance, dirtibution) {
  v = as.numeric(variance)
  m = as.numeric(moyenne)
  n <- NA
  if(dirtibution=="nbinom" && m<v){
    n <- (log((m/v)*m)) / log(1-(m/v))
  }else if(dirtibution=="binom" && m!=v){
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

  cat("\nObserved and fitted values for", attr(x,"dirtibution"),"distibution\n")
  cat("with parameter", if(attr(x,"dirtibution")=="poisson") " " else "s (", p(x$estimate,sep=", "), if(attr(x,"dirtibution")=="poisson") "" else ")", " estimated by ",p("`", p(attr(x, 'method'), sep=" "),"`"), "\n\n", sep='')

  print(data.frame(count=x$count, observed=x$observed, fitted=x$fitted,prob=x$prob), row.names=F, ...)
  cat("\nX-squared = ", x$Xsquared, ", df = ",x$df, ", p.value = ", x$p.value, sep='')
  cat("\nWe", if(x$accepthyp) "accept" else "reject","the null hypothesis")

  # print(p("\nObserved and fitted values for", attr(x,"dirtibution"),"distibution\n"))
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

# esmtimate= result[[dirtibution]]$estimate, Xsquared=result[[dirtibution]]$Xsquared, observed=result[[dirtibution]]$observed, count=result[[dirtibution]]$count,df=result[[dirtibution]]$df,fitted=result[[dirtibution]]$fitted, p.value=result[[dirtibution]]$p.value)

#if(detailed.curves) {plot(ecdf(x), main = );legend(x=11.5,y=0.35, legend=c("fn de r. de x", p("fn de r. de la dirtibution ", dirtibution) ), lty=1:2, col = c("black", color[[dirtibution]]))}

#pp<-ggplot(df, aes(x=x, y=y)) + stat_ecdf(aes(z), colour='black', linetype=2)
#ggdistribution(pgamma, seq(0, max(x),length.out= length(x)), shape=result$gamma$estimate[[1]],rate=result$gamma$estimate[[2]], colour = 'red', p=pp)
#pp
#pp <- ggplot( data=data.frame(var=c(distr, x) , col = c( )), aes(var, colour=col)) + stat_ecdf(size=1) + scale_colour_manual(values = c("#000000", "firebrick"))
#print(pp)
