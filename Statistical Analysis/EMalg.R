
EM.GMM <- function( x = NULL, G = 3, initpar = NULL, maxstep = 2000, tol = 1e-6 )
{
	# explanation of input
	# x : a vector of univariate observations
	# G : the number of groups to fit
	# initpar : an optional list with 3 vectors of length G giving initial 
	#				weights (wei), means (mu), sd (sd) for each component


	# check input
	if( is.null(x) || length(x) < 5 ) stop("Have you passed the data in correctly?")
	if( G < 1 ) stop("Please give a reasonable value of G.")
	if( maxstep < 50 ) stop("maxstep must be positive and it is recommended to set it larger.")
	if( tol < 0 ) stop("Tolerance threshold for termination should be positive.")
	if( !is.null(initpar) )
	{
		# what could we put in here?	
	}
	
	n <- length(x)
	
	# useful function 
	logsumexp <- function(t)
	{
	  #apply the log-sum-exp formula
	  tmax <- max(t)
	  logsumexp <- tmax + log( sum( exp(t-tmax) ) )
	  return(logsumexp)
	}
	
	estep <- function( w, m, v )
	{
		Smat <- matrix( nrow=n, ncol=G )
		for( g in 1:G )
		{
			Smat[,g] <- dnorm( x, m[g], sd=sqrt(v[g]), log=TRUE ) + log( w[g] )
		}
		lse <- apply( Smat, 1, logsumexp )
		Smat <- Smat - lse
		# exponentiate to get probabilities
		Smat <- exp(Smat)
		# compute the log-likelihood
		llike <- sum( lse )
		return( list( S=Smat, loglike = llike ) )
	}
	
	mstep <- function( Smat )
	{
		# pre-weight
		pw <- apply( Smat, 2, sum )
		# numerical fix for division by zero if component starvation occurs  
		if( any( pw < 1e-10 ) ) pw[ which( pw < 1e-10 ) ] <- 1e-10
		# weight
		w <- pw / n
		# means
		M <- Smat * x  # mulitplies each column of S by x
		m <- apply( M, 2, sum ) / pw
		# variances
		V <- Smat * x^2 # muliplies each column of S by x^2
		v <- apply( V, 2, sum ) / pw - m^2
		return( list( wei=w, mu=m, var=v ) )
	}

	## INITIALISATION ##
	# have initial values been given? If not, try to compute 
	#  sensible ones
	if( is.null( initpar ) )
	{
		wei <- rep( 1/G, G )
		mu <- numeric( G )
		mu[1] <- sample( x, size=1 )
		# kmeans++ initialisation
		dis <- rep( Inf, n ) 
		for( g in 2:G )
		{
			dispr <- ( x - mu[g-1] )^2
			w <- which( dispr < dis )
			dis[w] <- dispr[w]
			probs <- dis/sum(dis)
			mu[g] <- sample( x, size=1, prob=probs )
		}
		# initial S
		Mu <- matrix( rep(mu,n), nrow=n, byrow=T )
		lab <- apply( (x-Mu)^2, 1, which.min )
		Si <- matrix( 0, nrow=n, ncol=G )
		Si[ cbind( 1:n, lab ) ] <- 1
		initpar <- mstep( Si )
		# initial values of parameters
		wei <- initpar$wei
		mu <- initpar$mu
		var <- initpar$var
	}else{
		wei <- initpar$wei
		mu <- initpar$mean
		var <- initpar$sd^2
	}

	# initialize various controls
	oldllike <- -.Machine$double.xmax
	converged <- FALSE
	step <- 0
	
	S <- matrix( nrow=n, ncol=G )
	if( G == 1 ) S <- matrix( nrow=n, ncol=2 ) #special case


	## ALGORITHM ##

	while( !converged & step < maxstep  & G > 1)
	{

		## E-step ##

		est <- estep( wei, mu, var )
		S <- est$S
		llike <- est$loglike

		## M-step ##
		
		mst <- mstep( S )
		
		wei <- mst$wei
		mu <- mst$mu
		var <- mst$var
		
		# relative tolerance 
		reltol <- abs( ( llike - oldllike ) / oldllike ) 
		
		if( reltol < tol ) converged <- TRUE
		
		oldllike <- llike
		
		step <- step + 1
	
	} 

	if( G == 1 ) 
	{
	   S <- rep( 1, n )
	   wei <- 1
	   mu <- mean(x)
	   sd <- var(x) * (n-1)/n # MLE variance
	}

	z <- list( data = x, G = G, S = S, loglike = llike, par = list( wei = wei, mu = mu, sd = sqrt(var) ), converged = converged, nstep = step )

	return( z )
	
}

