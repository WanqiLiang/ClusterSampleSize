



ADEPT_sample_size = function(N = NULL, beta = NULL, alpha, m, delta, ICC, p_1, Cor_Y_X = NULL){
	## N is the total number of clusters
	## beta is the type 2 error (1 - power)
	## alpha is the type 1 error
	## delta is the standardized effect size
	## ICC is the inter cluster correlation (rho with no covariates and rho* with covariates)
	## p_1 is P(R = 1 | A_1 = 1)
	## Cor_Y_X is correlation between Y and the cluster level covariate X. It should only
	## be defined when there is a covariate. If it is left undefined the non-covariate formula
	## will be used
	
	## To use the formula, either N or beta should be left blank, but not both.
	## When N is blank, the sample size necessary to obtain that power will be returned
	## When beta is blank, the power will be returned for a given sample size.
	
	z_alpha = qnorm(1-alpha/2)  ## Get the standard normal quanitle
	cov_string = "with a cluster level covariate"
	if(Cor_Y_X == 0){  ## determines if we wil use the formula with no covariates
		cov_string = "with no covariates"
	}
	
	power_val = NULL
	sample_size = NULL
	
	
	## the case where we solve for N
	if(is.null(N)){
		z_beta = qnorm(1-beta)
		
		first_term = 4*(z_beta+z_alpha)^2/(m*delta^2)
		second_term = (1+(m-1)*ICC)
		third_term = 1+(1-p_1)/2
		
		# Sample size
		sample_size = ceiling(first_term*second_term*third_term*(1-Cor_Y_X^2))
		
		info = paste0("The sample size required for an ADEPT design ", cov_string, " is ", toString(sample_size))
		
		
		## the case where we solve for the power
	} else if(is.null(beta)){
		second_term = (1+(m-1)*ICC)
		third_term = 1+(1-p_1)/2
		
		# The power value by back solving the equation
		power_val = pnorm(sqrt(N/second_term/third_term/(1-Cor_Y_X^2)/4*delta^2*m) - z_alpha)
		
		power_val = round(power_val, digits = 4)
		info = paste0("The power for an ADEPT design ", cov_string, " is ", power_val)
		
	} else {
		info = "Either beta or N must be left undefined."
	}
	
	list(info = info, power = power_val, sample_size = sample_size)
}


prototypical_sample_size = function(N = NULL, beta = NULL, alpha, m, delta, ICC, p_1, p_neg1, Cor_Y_X = NULL){
	## N is the total number of clusters
	## beta is the type 2 error (1 - power)
	## alpha is the type 1 error
	## delta is the standardized effect size
	## ICC is the inter cluster correlation (rho with no covariates and rho* with covariates)
	## p_1 is P(R = 1 | A_1 = 1)
	## p_neg1 is P(R = 1 | A_1 = -1)
	## Cor_Y_X is correlation between Y and the cluster level covariate X. It should only
	## be defined when there is a covariate. If it is left undefined the non-covariate formula
	## will be used
	
	## To use the formula, either N or beta should be left blank, but not both.
	## When N is blank, the sample size necessary to obtain that power will be returned
	## When beta is blank, the power will be returned for a given sample size.
	
	z_alpha = qnorm(1-alpha/2)  ## Get the standard normal quanitle
	cov_string = "with a cluster level covariate"
	if(Cor_Y_X == 0){  ## determines if we wil use the formula with no covariates
		cov_string = "with no covariates"
	}
	
	power_val = NULL
	sample_size = NULL
	
	## the case where we solve for N
	if(is.null(N)){
		z_beta = qnorm(1-beta)
		
		first_term = 4*(z_beta+z_alpha)^2/(m*delta^2)
		second_term = (1+(m-1)*ICC)
		third_term = 1+(1-p_1 + 1-p_neg1)/2
		
		# Sample size
		sample_size = ceiling(first_term*second_term*third_term*(1-Cor_Y_X^2))
		
		info = paste0("The sample size required for a prototypical design", cov_string, " is ", toString(sample_size))
		
		
		## the case where we solve for the power
	} else if(is.null(beta)){
		second_term = (1+(m-1)*ICC)
		third_term = 1+(1-p_1 + 1-p_neg1)/2
		
		# The power value by back solving the equation
		power_val = pnorm(sqrt(N/second_term/third_term/(1-Cor_Y_X^2)/4*delta^2*m) - z_alpha)
		
		info = paste0("The power for a prototypical design", cov_string, " is ", power_val)
		
	} else {
		info = "Either beta or N must be left undefined"
	}
	list(info2 = info, power2 = power_val, sample_size2 = sample_size)
}