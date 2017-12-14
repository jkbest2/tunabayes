---
title: FISH 558 Final Project Proposal
author: John Best
bibliography: final.bib
---

# Introduction

Markov Chain Monte Carlo is a foundational technique for fitting Bayesian statistical models. In the natural sciences these models have often been fitted using the software packages that provide a domain-specific language that makes model specification straightforward. The BUGS family of software packages, including WinBUGS, OpenBUGS, and the more recent JAGS have been among the most popular in the natural sciences literature [@monnahan2017]. These packages use Gibbs sampling to produce autocorrelated samples from the posterior of a Bayesian model.

A more recent package is Stan [@stan2017]. Instead of using a Gibbs sampler, Stan uses a self-tuning version of Hamiltonian Monte Carlo (HMC) called the No-U-Turn Sampler (NUTS). Hamiltonian Monte Carlo uses an analogy to physical dynamics to generate proposals. Momenta are sampled along with parameter values, and the value of the log-posterior density serves as a measure of potential energy.

This generally provides samples with reduced autocorrelation as compared to Gibbs and Metropolis-Hastings type samplers. It can also be more effective in exploring the parameter space. This is done using the gradient of the posterior, calculated via automatic differentiation for accuracy and efficiency. Of course, the extra steps in generating the proposals in this manner require a larger computational burden than a Gibbs sampler, with multiple posterior density and gradient evaluations per proposal. Thus it may take longer to generate the same number of samples using Stan, but because autocorrelation is reduced, the effective sample size is larger and thinning is often unnecessary. This can result in more *effective* samples per unit time using Stan rather than a Gibbs implementation.

## Divergences

Using Hamiltonian dynamics to generate proposals requires numerically integrating the paths that the parameter particles follow in the posterior space [@betancourt2017]. These numerical integrals can sometimes be tripped up by gradients that are too steep, and so send particles jetting off to the infinities. These proposals are not accepted, and a warning of "divergent transitions" is issued by Stan [@betancourt2013].

These warnings are not ignored lightly. There is strong evidence that these are indicative of steep gradients in the posterior geometry that the sampler is not well-suited to explore. Our guarantees of convergence are only in the limit of infinite MCMC sample size. With a finite number of samples it is likely that in these areas of high curvature we are failing to explore an area of the parameter space adequately, resulting in biased estimators. The Gibbs samplers used in the BUGS family of software packages will have similar difficulties exploring these regions of the parameter space, but will not give any indication that the results may be biased.

## Fixes for divergences

The quick and dirty fix for divergences is to reduce the size of the time step that the integrator is using; if the gradient isn't too extreme the proposals will stop "overshooting" and the sampler will be able to explore the full parameter space. This is done via the `adapt_delta` control argument to Stan. This results in more computation time within each iteration, but this is worth it for unbiased samples.

For some models however, the gradients increase essentially without bound, as in the classical hierarchical regression model (e.g. 8 schools).In this case, the only way to deal with the divergences is to modify the model to eliminate these regions of extreme curvature. This can be done via the so-called non-centering approach, where parameters are sampled from a standard distribution, and then their location and scale are adjusted as necessary so that they can be used in the posterior calculations [@monnahan2017, @divcase].


# Methods

## Models

I will focus on the Bayesian model that we looked at in Lecture 4. First I will reproduce the fits in JAGS as we used them in class. As this is a hierarchical model, there is probably some pathological posterior geometry [@betancourt2013].

Next I will translate the model to the Stan language. A naïve translation will yield a model that should be directly comparable to that fit in JAGS. I will also time the fit for this model, and calculate the effective sample size. Diagnostic information on divergences and other MCMC issues will also be recorded and presented.

Finally, I will modify the naïve Stan model using the non-centered parameterization in an attempt to eliminate the divergences I expect to encounter in the naïve Stan model. This will take some consideration, as this model uses a hierarchical log-normal likelihood rather than a normal likelihood. I will also explore further optimizations where possible.

## Model runs

In setting the number of iterations and chains for each model run, I will need to balance time with a need to thoroughly explore the parameter space to really ensure that I find areas with divergent transitions. It is also important that I give the JAGS model a sufficient number of iterations to explore the parameter space as effectively. This may take some tuning.

In order to get comparable timings, I will not run Stan chains in parallel for the timing results (described below). Stan models are compiled, so it will also be important to get separate timings for compilation and sampling.

## Model comparisons

My first point of comparison is the number of divergences in each Stan model, with the goal of eliminating them completely in the optimized Stan model. Once the divergences are eliminated, I will compare the posterior distributions of the three models. There is a possibility of finding evidence of bias in the JAGS and naïve Stan formulations. Point estimates including marginal means and medians will be compared to look for bias, while considering the Monte Carlo error associated with these estimates.

After this, it will be interesting to compare the effective sample size per clock time of each formulation. I expect that the naïve Stan model will out-sample the JAGS model. It is also possible that the optimized Stan model will out-sample the naïve Stan model. This is based on the "folk theorem of statistical computing", as stated by Andrew Gelman [@folktheorem2008].

I will calculate effective sample size using both the `rstan` and `coda` packages. Each package calculates effective sample size differently. The `rstan` version is meant to be more conservative.

# Bibliography
