
# r_sim assumes r is lognormally distributed with parameters mu and sigma
# mu is constant for all firms
# sigma varies with firm size based on regressions on compustat data

# inputs:
# emp = compustat employment
# r = fitted r value for each compustat firm
# firm.sim = simulated firm size distribution


# lnorm same modes
r_sim_1 = function(emp, r, firm.sim, bin_factor){

    r = r -1

    # regression for sigma.r
    dt = data.table(emp, r)

    dt$group = round( bin_factor*log(dt$emp) )
    sigma = dt[, sd(log(r)), by = group]

    X =  na.omit(    cbind(rep.int(1, length(sigma$group)), sigma$group, sigma$V1)  )
    regres.s = fastLmPure(X[,1:2], X[,3])

    # get sigma for each firm
    sigma.r =   coef(regres.s)[1] + coef(regres.s)[2]*log(firm.sim)

    # get mu for each firm (same mode for all firms)
    dens = density(r)
    mode.r = dens$x[which.max(dens$y)]
    mu.r = log(mode.r) + sigma.r^2


    # simulated r dist
    l = length(firm.sim)
    r.mod = exp(sigma.r*zrnorm(l) + mu.r) + 1

    return(r.mod)
}




# lnorm same medians
r_sim_2 = function(emp, pay.scaling, firm.sim, bin_factor){

    r = pay.scaling -1

    # regression for mu.r
    mu.r = rep(mean(log(r)) , length(firm.sim))

    # regression for sigma.r
    dt = data.table(emp, r)

    dt$group = round( bin_factor*log(dt$emp) )/bin_factor
    sigma = dt[, sd(log(r)), by = group]

    regres.s = lm(sigma$V1 ~ sigma$group)
    sigma.r =   coef(regres.s)[1] + coef(regres.s)[2]*log(firm.sim)

    l = length(firm.sim)

    r.sim = exp(sigma.r*zrnorm(l) + mu.r) + 1
    return(r.sim)
}





# lnorm mu declines with firm size
r_sim_3 = function(emp, pay.scaling, firm.sim, bin_factor){

    r = pay.scaling -1

    # regression for mu.r
    regres.mu = lm(log(r) ~ log(emp))
    mu.r =  coef(regres.mu)[1]  +  coef(regres.mu)[2]*log(firm.sim)

    # regression for sigma.r
    dt = data.table(emp, r)

    dt$group = round( bin_factor*log(dt$emp) )/bin_factor
    sigma = dt[, sd(log(r)), by = group]

    regres.s = lm(sigma$V1 ~ sigma$group)
    sigma.r =   coef(regres.s)[1] + coef(regres.s)[2]*log(firm.sim)

    l = length(firm.sim)

    r.sim = exp(sigma.r*zrnorm(l) + mu.r) + 1
    return(r.sim)
}



# beta sim
# mu is constant
# sigma is power function of firm size
beta_sim_R = function(emp, beta, firm.sim, bin_factor){

  # regression for mu_beta
  mu_beta = rep(mean(log(beta)) , length(firm.sim))

  # regression for sigma.r
  dt = data.table(emp, beta)

  dt$group = round( bin_factor*log(dt$emp) )/bin_factor
  sigma = dt[, sd(log(beta)), by = group]

  # log-log regression
  sigma = sigma[V1 > 0]
  regres.s = lm(log(sigma$V1) ~ sigma$group)
  sigma.beta = exp(   coef(regres.s)[1] + coef(regres.s)[2]*log(firm.sim) )

  l = length(firm.sim)

  beta.sim = rlnorm(l, mu_beta, sigma.beta)

  return(beta.sim)
}



