---
title: Model parameterizations for Bayesian nonlinear state space models
author: John Best
abstract: Bayesian state-space models are common in fisheries, but even relatively simple models can be computationally expensive to fit, and diagnosing poor fits can be difficult. The Stan software package provides an advanced MCMC sampler and diagnostics that are not available in older packages. Here we compare sampler diagnostics, efficiency, and posterior inferences among different parameterizations of a state-space model. We find that compromises made in the past for computational reasons are now a liability, and may have resulted in biased inferences. These issues are also apparent in the sampler diagnostics now available from Stan. Choosing the appropriate parameterization of a model can increase performance and make inferences more robust.
geometry: margin=1in
fontsize: 10pt
latex-engine: xelatex
numbersections: true
---

# Introduction

Bayesian state-space models are used extensively in fisheries. Details of how these models are implented and fit can have a substantial effect on the computational costs and the trustworthiness of the resulting inferences. An early example of using general-purpose Markov chain Monte Carlo (MCMC) software to fit nonlinear state-space models for fisheries is presented in @Meyer1999, and the implementation in their appendix has formed the basis for many stock assessments around the world. Advances in MCMC algorithms have made new diagnostics available. These indicate that the implementation of @Meyer1999 may be problematic.

Baysian state-space models are often used for stock assessments of species where data is limited to catches and indices of abundance such as catch per unit effort. These include pelagic species such as swordfish [@ICCAT2014; @ICCAT2017a], blue shark [@ICCAT2016; @ISC2017], shortfin mako [@ICCAT2017], and albacore [@ICCAT2017b]. They are also used for invertebrates such as tiger prawns in Australia [@Zhou2010] and marine mammals such as Humpback whales in the southern Atlantic [@Zerbini2011]. These models also underpin multiple software packages used for stock assessment, including BSP2 [@McAllister2014] and JABBA [@Winker2018].

Linear state-space models can be solved analytically [@Kalman1960], and have found use in many areas. Biomass dynamics models used in fisheries models are nonlinear, so analytic solutions are not available. A common approach to fitting Bayesian models is to use Markov chain Monte Carlo (MCMC). These methods require software that can be a research project in its own right, so many practitioners use general purpose MCMC packages. The BUGS software package [@Gilks1994] was an early example of this kind of software. While its popularity has declined in recent years, updated versions such as WinBUGS [@Lunn2000], OpenBUGS [@Lunn2009], and JAGS [@Plummer2003] have seen wide use across scientific disciplines [@Monnahan2017]. The recent JABBA package for R [@Winker2018] provides a convenient interface to fit nonlinear state-space models for fisheries in JAGS. A newer software package for general purpose MCMC is Stan [@Carpenter2017]. Its use is becoming increasingly widespread [@Monnahan2017].

Biomass dynamics models such as the Schaefer model [@Schaefer1954] specify a functional relationship for population change between consecutive years. These models are typically chosen for their simplicity and for their behavior in the regimes we typically see, as when the population is at or below carrying capacity. Surplus production models can exhibit pathological behavior in some situations, which in this case manifests as predictions of negative biomass for subsequent time steps. In the case of the Schaefer model used here, this can occur if the current population is less than the observed catch, or if the current population is so large that density dependence predicts a negative surplus production larger in magnitude than the current population.

Setting any negative predictions to zero reaches a fixed point of the surplus production model, and indicates that the population has gone extinct. However, given records of positive catch after a given year, we know *a priori* that the population must have had at least as much biomass as was caught that year. Pracically, this is also important when using log Normal process and observation errors (as in the models presented here), because the natural $\log$ of the predicted populations and indices are taken. Multiple "fixes" are used to address this. The most common approach is to fix negative or zero predictions to a small value. This is typically done arbitrarily, and does not address other paradoxes such as predicting less biomass than was actually observed through catches in subsequent years. It also does not address situations where density dependence results in negative predictions. Sometimes upper bounds are added to prevent this These are also typically chosen arbitrarily. Another approach is to consider that catch is observed with some error, and to fit a fishing mortality rate for each year. This ensures that true catch is less than the predicted biomass. The effect of using each of these approaches on model fitting will be explored.

Stan uses the No-U-Turn Sampler (NUTS) [@Hoffman2014]. This a self-tuning variant of Hamiltonian Monte Carlo [@Neal2011], which uses an analogue to a physical system to generate MCMC proposals that more effectively explore the parameter space than techniques like Gibbs sampling or random-walk Metropolis sampling. MCMC that uses NUTS also provides additional diagnostic tools, allowing practitioners to both check whether areas of the parameter space are being missed and to tune the computational efficiency of the fitting process.

Hamiltonian Monte Carlo uses the gradient of the log posterior density to generate proposals by solving a system of ordinary differential equations through parameter space. This system of ordinary differential equations is solved numerically by discretizing the path into a series of steps. When this integration breaks down. Error due to discretization can cause the proposal to enter infeasible areas of the parameter space. This is particularly the case when the gradient changes quickly, and is known as a *divergent transition*.

To reduce the number of log posterior density evaluations to generate a proposal, the proposal paths are built up sequentially. First a single step is taken, then two, doubling each time until a "U-turn" occurs. The number of doublings is limited. The second type of diagnostic is indicated when the maximum number of doublings is met, and is known as a *maximum treedepth exceedence*. This essentially indicates that some area of the posterior is too flat to be efficiently explored using the time step chosen during the warmup and adaptation phase. Note that just because these diagnostics are only available when using NUTS does not mean that the issues they indicate are not present when using other samplers. For an introductory description of Hamiltonian Monte Carlo and NUTS, see @Monnahan2017. @Betancourt2017a provides an in-depth description of both the theory and implementation of these algorithms.

When the model implemented in @Meyer1999 was translated from BUGS to Stan, an alarming number of warnings about divergent transitions were issued. At the time it was written, BUGS did not have a general purpose Metropolis algorithm. Parameters that did not have conjugate priors were required to have log-concave full conditional distributions for adaptive rejection sampling. To do this, @Meyer1999 truncated the distributions of each parameter. These truncations proved to be a liability when fitting the model in Stan. These hard bounds are problematic because they are not differentiable.

However, hierarchical models often suffer from divergent transitions, and techniques such as noncentering random effects [@Papaspiliopoulos2007; @Betancourt2015]. This kind of reparameterization can reduce parameter correlation and gradient changes through the parameter space. Other parameterizations commonly used in fisheries may marginalize out one or more parameter, as presented in @Walters1994. Explicitly esimating fishing mortality is another option, with the added benefit that predictions of negative biomass are prevented.

Here we fit seven parameterizations of a Schaefer biomass dynamics model to a data set of catch and effort from the South Atlantic albacore fishery. We find that the compromises made for computational reasons in @Meyer1999 are no longer necessary when the model is fit using modern MCMC software, resulting in both slower fitting and a substantively different posterior. Of the other parameterizations five of the six have potential uses depending on what prior information is available. The parameterization as presented (but not actually fit) in @Meyer1999 performs well overall.
  
# Methods

## Model specification

The nonlinear Bayesian state-space model is specified as a hierarchical model. The observed catch per unit effort $I_t$ at time $t = 1,\dots,T$ is assumed to be proportional to the true abundance through the catchability coefficient $q$. Further, it is assumed to be observed with log-normal error with variance $\tau^2$, so that

$$I_t \mid B_t, q, \tau^2 \sim \textrm{log Normal}\left(\log\left[ q B_t \right], \tau^2\right) \quad t = 1,\dots,T$$

The Schaefer biomass dynamics model [@Schaefer1954] is used here, following @Meyer1999. Intrinsic growth rate is denoted $r$ and carrying capacity $K$. $T$ years of catch data are available. Biomass at time $t$ is $B_t$. In order to separate the estimation of dynamics from carrying capacity, depletion is the estimated quantity. Depletion at time $t$ is denoted $P_t$, where $P_t = B_t / K$. Observed catch at time $t$ is $C_t$. Multiplicative (log-normal) process error with variance $\sigma^2$ is assumed for each year, with independence among years. Median depletion of the unfished population (at $t = 1$) is assumed to be $1$. Log-normal independent process error with variance $\sigma^2$ is included for each year. The population dynamics process is then

$$\tilde{P}_1 = 1$$
$$\tilde{P}_{t} = P_{t-1} + r P_{t-1} (1 - P_{t-1}) - \frac{C_{t-1}}{K} \quad t=2,\dots,T$$
$$P_{t} \mid P_{t-1}, \sigma^2 \sim \mathrm{log Normal}\left(\log \tilde{P}_t, \sigma^2\right) \quad t=1,\dots,T.$$

Priors match those used in @Meyer1999, which were based on a review of the literature, and an attempt to match particular quantiles (described in the appendix of @Meyer1999). The only noninformative prior is that for catchability. These are

$$r \sim \textrm{log Normal}(-1.38, 0.51^2)$$
$$K \sim \textrm{log Normal}(5.04, 0.5162^2)$$
$$p(q) \propto 1/q$$
$$\sigma^2 \sim \textrm{Inverse Gamma}(3.79, 0.0102)$$
$$\tau^2 \sim \textrm{Inverse Gamma}(1.71, 0.0086).$$

The posterior distribution of the parameter values combines the information in each of these components. For MCMC applications, we only need the posterior up to a constant of proportionality. Observation likelihoods are assumed independent conditional on the process model, and process errors are assumed independent as well. In terms of the probability density functions $p(\cdot)$ of the distributions defined above, we have

$$p(r, K, q, \boldsymbol{P}, \sigma^2, \tau^2 \mid \boldsymbol{I}) \propto
\prod_{t=1}^{T} p(I_t \mid P_t, K, q, \tau^2)$$
$$\times\ p(P_1) \times \prod_{t=2}^{T} p(P_t \mid r, K, P_{t-1}, \sigma^2)$$
$$\times p(r)\ p(K)\ p(q)\ p(\sigma^2)\ p(\tau^2)$$

In practice, posterior densities are calculated in log-space to avoid issues of numerical underflow. Though the statistical model is fully defined here, there are multiple potential parameterizations and impletmentation details to consider. Each has specific performance characteristics of the fitting procedure. The following parameterizations are summarized in Table {@tbl:param}.

### Centered model

We used six parameterizations of the model. The *centered* model is a translation of the BUGS code in the appendix of @Meyer1999 to idiomatic Stan code. It corresponds directly to the model specified above. To avoid estimates of negative depletion and attempting to take the log of a negative number, a lower bound of $0.001$ is placed on the median depletion. The state likelihood is

### Truncated model

The *truncated* parameterization is a direct translation of the model *actually fit* in @Meyer1999 to Stan. This required adding truncations to the priors on $r$, $K$, and $1 / q$. These were added to allow the BUGS software available at the time to sample from a log-concave full conditional posterior distribution. Note that the specified prior on $q$ was approximated using a $\textrm{Gamma}$ distribution here. The $\textrm{Inverse\ Gamma}$ distribution was not available in BUGS, so $\textrm{Gamma}$ priors were placed on the process precision and observation precision. Truncations were not specified for these parameters. Priors were specified as

$$r \sim \textrm{log Normal}(-1.38, 1 / 3.845) \quad 0.01 < r < 1.2$$
$$K \sim \textrm{log Normal}(5.042905, 1 / 3.7603664) \quad 10 < K < 1000$$
$$1 / q \sim \textrm{Gamma}(0.001, 0.001) \quad 0.5 < 1 / q < 100$$
$$\sigma^{-2} \sim \textrm{Gamma}(3.785518, 0.010223)$$
$$\tau^{-2} \sim \textrm{Gamma}(1.708603, 0.008613854).$$

The depletion parameters (the state variables) were also assigned truncated priors, where

$$P_t \sim \textrm{log\ Normal}(\log \tilde{P}_t, \sigma^2) \quad 0.01 < P_t < 2 \quad t = 1,\dots,T.$$

### Constrained depletion

Preventing negative depletions is both practically important, allowing us to take the $\log$, and conceptually important. A prediction that results in a negative depletion due to a combination of natural and harvest mortality implies that the population has been driven extinct. However, if the population of interest has seen continuous harvest, it must not have gone extinct. This knowledge allows us to calculate bounds on the depletion parameters based on this fact.

For values of $P_{t-1}$, $r$, $K$, and $C_{t-1}$, we want bounds such that $P_{t} > 0$. Due to the quadratic nature of the Schaefer model, both a lower and upper bound will be required. The lower bound ensures that sufficient biomass is available for the observed catch. The upper bound ensures that density dependence at high depletions (much greater than one) will not result in a prediction of negative biomass. The upper bound is unlikely to come into play in a fisheries system, but eliminating these values may help the sampler stay within the desired range.

To find these bounds, we set

$$P_{t-1} + r P_{t-1} (1 - P_{t-1}) - \frac{C_{t-1}}{K} > 0.$$

Using the quadratic formula we find that

$$\frac{1 + r - \sqrt{(1 + r)^2 - 4 r C_t / K}}{2r} < P_{t-1} < \frac{1 + r + \sqrt{(1 + r)^2 - 4 r C_t / K}}{2r}$$

Ensures that the subsequent predicted median depletion, $\tilde{P}_{t}$, will be nonnegative. These constraints are included in the Stan program with the required Jacobian correction.

### Noncentered process noise

Noncentering [@Papaspiliopoulos2007] is a technique that is commonly used in additive hierarchical models to improve sampler efficiency and reduce potential for parameter bias [@Betancourt2015]. Noncentering decorrelates the sampled parameters without changing the posterior. In this case, we noncenter the process noise, so that the biomass dynamics take the form

$$P_1 = \exp(\sigma u_1)$$
$$P_{t} = \left[P_{t-1} + r P_{t-1} (1 - P_{t-1}) - \frac{C_{t-1}}{K}\right]\exp(\sigma u_t) \quad t=2,\dots,T$$
$$u_t \stackrel{\textrm{iid}}{\sim} \textrm{Normal}(0, 1) \quad t = 1,\dots,T.$$

### Marginalized catchability

The prior on catchability takes a form that is amenable to marginalization. Following @Walters1994, we calculate

$$Z_t = \log\left(\frac{I_t}{P_t K}\right) \quad t = 1,\dots,T$$
$$\hat{q}' = \frac{\sum_t Z_t}{T},$$ {#eq:margq_vals}

and

$$Z_t \stackrel{\text{iid}}{\sim} \textrm{Normal}(\hat{q}', \tau^2).$$ {#eq:margq_lik}

Catchability is then not estimated at all in this model, and so the prior on $q$ is also removed from the model. The rest of the structure remains the same.

### Explicit fishing mortality

Another way to eliminate predictions of negative depletion is to estimate fishing mortality rate for each year, and include the likelihood of the observed catch given the instantaneous fishing mortality. There is often no information on the variance of catch reports, so here we assume a normal likelihood with a fixed variance. We also consider the sensitivity of our final inferences to the fixed variance value.

Because catch is estimated here, it is important to consider the biomass pool that is being fished. Here we assume that fishing occurs on the biomass pool *after* production has occurred. For instantaneous fishing mortality $F_t$, this gives biomass dynamics

$$\tilde{P}_1 = 1$$
$$\tilde{P}_{t} = \left[P_{t-1} + r P_{t-1} (1 - P_{t-1})\right] - \left[P_{t-1} + r P_{t-1} (1 - P_{t-1})\right] \exp(-F_{t-1}) \quad t = 2,\dots,T,$$
  
where the second equation simplifies to

$$\tilde{P}_{t+1} = \left[P_t + r P_t (1 - P_t)\right](1 - \exp(-F_t)).$$ {#eq:exF_dyn}

In this way the predicted depletion can only be negative due to density dependence, because fishing only ever takes a *proportion* of the exisiting biomass. Catches are assumed to be observed with additive, normally-distributed error, with fixed catch variance $\xi^2$, so that

$$C^*_t = \left[P_t + r P_t (1 - P_t)\right] \exp(-F_t),$$
$$C_t \sim \textrm{Normal}(C^*_t, \xi^2) \quad t=1,\dots,T.$$ {#eq:exF_catchlik}

Each $F_t$ receives the same prior,

$$F_t \stackrel{\text{iid}}{\sim} \textrm{Flat}(0, \infty) \quad t = 1,\dots,T.$$

### Explicit fishing mortality with marginalized catchability

When fitting the previous model, it was clear that the limiting parameter (in terms of effective sample size) was the catchability parameter $q$. The final model parameterization combines the previous two, estimating instantaneous fishing mortality while marginalizing out catchabilty. That is, it uses Equation @eq:exF_dyn for population dynamics and the likelihood is calculated using Equations @eq:margq_vals, @eq:margq_lik, and @eq:exF_catchlik. Other priors remained the same as the /centered/ parameterization, with $q$ obviously excluded.

## Data

The data set has been published multiple times, including in @Polacheck1993.  and @Meyer1999. It includes 23 years of observed catch and catch per unit effort data from the south Atlantic albacore fishery between 1967 and 1989. Catch is provided in thousands of tons while catch per unit effort has units of kilograms per 100 hooks. A classic "one way trip" is exhibited where catch-per-unit-effort decreases over time (Figure @fig:catch_cpue).

## Fitting the different parameterizations

Each parameterization was fit using Stan 2.18 through the `rstan` interface [need a citation here]. Six chains of $20,000$ iterations each were run, with the first $5,000$ designated as warmup and not considered for inference. This leaves $90,000$ samples to be used for inference.

Chains were run first with the default sampler parameters. This uses a target acceptance rate (`adapt_delta`) of $0.8$ and a maximum treedepth of $10$. Initial runs were used to diagnose potential issues in each parameterization. All of the fits besides the two *explicit F* parameterizations displayed warnings about divergent transitions. These were fit a second time, increasing the target acceptance rate to $0.975$. The two *explicit F* parameterizations displayed warnings about exceeding maximum treedepth. These were fit a second time with maximum treedepth increased to $15$.

The *explicit F* parameterizations require a fixed catch variance. Based on initial runs, this was fixed at $\xi^2 = 0.2^2$. This was near the center of the set of sensitivity values considered below. It represents a prior belief that catch was within $\pm 0.4$Gg of the reported value.

## Catch error sensitivity

To check the sensitivity of the posterior distribution to the value of the catch variance, the *explicit F* parameterization was re-fit using a range of values for $\xi^2$. Ten values were chosen, space logarithmically from $\xi = 10^{-2}$ to $\xi = 10^2$. These were fit using a maximum tree depth of 15, and the same chain specifications used above.

# Results

We fit seven parameterizations of the state-space model. Each model was fit using the default sampler settings of Stan's NUTS sampler, and then again with adjustments based on diagnostics that indicated divergent transitions or trajectories that exceeded the maximum tree depth. We would like to identify parameterizations and sampler parameters that do not exhibit divergent transitions during sampling, and are as efficient as possible.

## Sampler diagnostics

Table {@tbl:fit_diag} shows the diagnostics for each of the model fits under both the default sampler parameters and the adjusted parameters. Except for the two *explicit F* parameterizations, the default target acceptance rate of $0.8$ resulted in divergent transitions. Increasing the target acceptance rate to $0.975$ eliminated the divergent transitions in the *centered*, *constrained P*, and *marginal q* parameterizations. The *truncated* parameterization showed a relatively small decrease in number of divergent transitions, but over half of the transitions remained divergent. The *noncentered* parameterization showed an order of magnitude decrease in number of divergent transitions, but this came at the expense of a number of transitions that exceeded the maximum treedepth.

The two *explicit F* parameterizations exhibited no divergent transitions, even under the default target acceptance rate. The flattening effect of this parameterization was evident in the large number of transitions that exceeded the default maximum tree depth of $10$. Increasing the maximum treedepth to 15 eliminated these warnings.

The number of effectively independent samples varied over multiple orders of magnitude among parameterizations. Table {@tbl:ess} shows that the *noncentered* parameterization generated the largest number of effectively independent samples under both the default and adjusted sampler parameters. The *centered* parameterization performed well, and was comparable to the *marginal q* parameterization after the target acceptance rate was increased.

## Parameterization timings

Although adjusting the target acceptance rate and maximum treedepth tended to increase the effective sample size, this came at the cost of increased computation time. The *marginal q* parameterization was generally the most efficient, producing around 60 effectively independent samples per second after adjusting the target acceptance rate. The *centered* and *noncentered* parameterizations were competitive with each other. The *constrained P* parameterization was next, with fewer than ten independent samples per second. The two *explicit F* parameterizations each produced around 2 independent samples per second. The *truncated* parameterization was consistently the least efficient, producing fewer than one independent sample per second.

Adjustments to the target acceptance rate and maximum treedepth generally slowed the sampling rate. The exceptions were the *centered* and *truncated* parameterizations, which each saw an increase. These increases are likely due to the decrease in divergent transitions, allowing the sampler to more effectively explore the parameter space.

## Parameterization posteriors

Most of the parameterizations exhibit strikingly similar posterior distributions (+@fig:biopost). The *truncated* parameterization is the one exception. Credible intervals for each year's biomass are much wider, and the median values differ, sometimes substantially. Posteriors of derived quantities of interest to managers show a similar pattern (+@fig:mgt_quant). Most worryingly, the *truncated* parameterization results in values that are biased high compared to the other parameterizations, potentially leading to management decisions that are unsustainable.

## Catch sensitivity

### Diagnostics

*@tbl:csd_diag demonstrates the influence of the fixed value of $\xi$ on the posterior. The magnitude of the fixed catch error clearly influences the geometry of the posterior. Smaller values result in posteriors with less curvature, as evidenced by the transitions that exceeded maximum tree depth. Large values of catch error result in a posterior with extreme curvature, potentially due to unidentifiability. It is possible that informative priors on the $F_t$ would alleviate this. Note also that the large values are particularly extreme, and fail to rule out absurdities such as negative catches.

The chains have clearly not converged to the posterior distribution for values of $\xi$ greater than $4$. The rest of this analysis will not consider these values. The values under consideration then span over three orders of magnitude, and represent the reasonable range that a practitioner might consider.

### Timing

The change in posterior geometry demonstrated above also influences sampling efficiency. The rate of sampling effectively independent samples increased roughly linearly with catch error in the range where sampling was successful (+@tbl:csd_timing).

### Posteriors

In the range where sampling was successful, the magnitude of assumed catch error does not appear to have had much effect on the posterior distributions of interest. The biomass series, as seen in +@fig:csd_biomass, does not show systematically different credible intervals or median estimates of the biomass for each year. Estimates of MSY, FMSY, and depletion in 1990 appear similarly unaffected by the magnitude of catch error (+@fig:csd_mgt).

# Discussion

The posterior marginal distributions of parameters estimated under the *truncated* parameterization are substantially different from those estimated under the other parameterizations.

The *centered* parameterization runs efficiently, and divergent transitions can be eliminated by adjusting the target acceptance rate. The efficicncy of the *noncentered* parameterization was comparable to the *centered* parameterization, but divergent transitions were not eliminated even at the fairly high target acceptance rate of $0.975$. We do not recommend using the *noncentered* parameterization. The *constrained P* has the advantage of biologically-motivated constraints to prevent predictions of negative depletion, but this also complicates the code and substantially decreases sampler efficiency. Using only the lower constraint from this parameterization may be useful in some cases.

The *truncated* parameterization performed poorly across the board. Relative to the other parameterizations, posterior distributions were less concentrated and generally biased upward. This could result in incautious management decisions. It is important to note that this parameterization was formulated for use in software that is now multiple generations behind the state of the art. Pracitioners looking to apply techniques from software of a certain age should be aware of compromises such as these and determine if they are still relevant. In this case the compromises that were necessary to fit the model in @Meyer1999 were a liability when the model was rewritten for Stan.

Unsurprisingly, marginalizing out catchability resulted in the most efficient parameterization. This analytic marginalization only works if we assume the improper $q \propto 1 / q$ prior. If prior information is available about catchability, or if it is a quantity of interest in its own right, the cost of using the *centered* parameterization is justified.

The *explicit F* parameterizations require estimating an additional parameter for every year of catch data and estimating or providing an estimate of catch error. If information is available on catch error, this would be an appropriate model, but it can be used with a fixed error over a few orders of magnitude without changing the posterior substantially. We did not attempt to set a hyperprior on catch error here, as we did not have prior information on it and did not expect that our data set would have much either. An alternative to our fixed strategy here would be to place for example an exponential prior with most of the probability mass below some limit.

If a pracitioner encounters warnings about divergent transitions, they should first see if increasing the target acceptance rate eliminates them. If the divergent transitions cannot be eliminated by increasing the target acceptance rate, alternative parameterizations should be considered. Warnings about exceeding the maximum tree depth should be eliminated by increasing the relavant sampler parameter. Note that either of these will most likely decrease sampler efficiency (though not always). Because Stan is compiled, it can be easy to run multiple short chains to find the sampler parameters that eliminate warnings while preserving sampler efficiency.

# References

\noindent
\vspace{-2em}
\setlength{\parindent}{-0.5in}
\setlength{\leftskip}{0.5in}
\setlength{\parskip}{5pt}

<div id="refs"></div>

\newpage

|                    | Process error | Truncated pars.  | $\boldsymbol{P} > 0$ | Marginalize $q$ |
| :----------------- | :-----------: | :-------------: | :------------------: | :-------------: |
| Centered           | C             |                 | Small val.           |                 |
| Truncated          | C             | $r,K,q^{-1}$    | Arb. trunc.          |                 |
| Constrained P      | C             |                 | Pos. trunc.          |                 |
| Noncentered        | NC            |                 | Small val.           |                 |
| Marginal q         | C             |                 | Small val.           | ×               |
| Explicit F         | C             |                 | Expl. F              |                 |
| Explicit F marg q  | C             |                 | Expl. F              | ×               |

Table: Features of each parameterization used in this study. The first column indicates whether process error was centered (C) or noncentered (NC). The next column indicates whether the biomass dynamics parameters were assigned truncated priors. The "$\boldsymbol{P} > 0$" column indicates the strategy used to prevent predictions of negative depletion, either by a small positive biomass to negative predictions (Small val), arbitrarily truncating the distribution of the predictions (Arb. trunc.), truncating above and below at the values where depletion becomes negative (Pos. trunc.), or including explicit fishing mortality (Expl. F). The final column indicates whether catchability ($q$) was analytically marginalized out. {#tbl:param}

\newpage

|                  | Divergences| Divergences Adj| Exc Treedepth| Exc Treedepth Adj|
|:-----------------|-----------:|---------------:|-------------:|-----------------:|
|Centered          |          89|               0|             0|                 0|
|Truncated         |       57990|           55030|             0|                 1|
|Constrained P     |          23|               0|             0|                 0|
|Noncentered       |        5639|             626|             0|                 0|
|Marginal q        |         102|               0|             0|                 0|
|Explicit F        |           0|               0|         11383|                 0|
|Explicit F marg q |           0|               0|         11162|                 0|

Table: Sampler diagnostics for each model parameterization. These counts are out of $90,000$ inference samples. The *centered*, *constrained P*, and *marginal q* parameterizations all responded well to increasing the target acceptance rate; this successfully eliminated all of the divergent transitions. The two *explicit F* parameterizations similarly responded well to an increased maximum treedepth. Adjusting the target acceptance rate did not eliminate all divergences for the *truncated* and *noncentered* parameterizations. {#tbl:fit_diag}

\newpage

|                  | Default| Adjusted|
|:-----------------|-------:|--------:|
|Centered          |    8237|    13120|
|Truncated         |     209|      607|
|Constrained P     |    7135|     5678|
|Noncentered       |   17343|    29182|
|Marginal q        |   13741|    13481|
|Explicit F        |    6561|     7110|
|Explicit F marg q |    6165|     6408|

Table: The estimated number of effectively independent samples from each parameterization. This is out of $90,000$ samples. Adjusting the sampler parameters increased effective sample size in all parameterizations except *marginal q* and *constrained P*. These increases are generally offset by increased sampling time (see figure @fig:sampler_efficiency). {#tbl:ess}

\newpage

| Catch error | Exc. Treedepth | Divergence | Minimum ESS |
|-------------|----------------|------------|-------------|
|       0.010 |           3245 |          0 |        8011 |
|       0.028 |              0 |          0 |        7526 |
|       0.077 |              0 |          0 |        7896 |
|       0.215 |              0 |          0 |        6742 |
|       0.599 |              0 |          0 |        8230 |
|       1.668 |              0 |         10 |        7008 |
|       4.641 |              0 |      89930 |           3 |
|      12.915 |              0 |      90000 |           3 |
|      35.938 |              0 |      90000 |           3 |
|     100.000 |              0 |      90000 |           3 |

Table: Diagnostics for *explicit F* parameterization fit with varying levels of catch error ($\xi$ in +@eq:exF_catchlik), fit with maximum tree depth increased to 15. Low error in catches results in transitions that exceed maximum treedepth. Neither divergences or maximum treedepth warnings are encountered for intermediate values of catch error. At high catch error, the model is not identifiable and no useful samples are returned. {#tbl:csd_diag}

\newpage

| Catch error| Min ESS Rate|
|-----------:|------------:|
|       0.010|        0.107|
|       0.028|        0.287|
|       0.077|        0.856|
|       0.215|        1.805|
|       0.599|        5.631|
|       1.668|       13.387|

Table: Effectively independent samples were produced more efficiently with a higher catch error, until sampling at values of catch error larger than those included here. {#tbl:csd_timing}

\newpage

![Catch and CPUE series from South Atlantic albacore fishery between 1967 and 1989. While catches have remained fairly constant, CPUE shows a marked decline.](figs/catch_cpue.png){#fig:catch_cpue}

\newpage

![Sampler efficiency under different parameterizations. Except for the *centered* and *noncentered* parameterizations, the efficiency rankings hold before and after adjusting the target acceptance rates and maximum treedepths. Most models showed a decrease after these adjustments. The *centered* and *truncated* parameterizations showed an increase.](figs/sampler_efficiency.png){#fig:sampler_efficiency}

\newpage

![Posterior biomass series under each parameterization. Median values, 50%, and 80% credible intervals are plotted. The *truncated* parameterization exhibits substantially larger credible intervals and more variable median values. The posterior distributions under the other parameterizations are almost indistinguishable.](figs/biopost.png){#fig:biopost}

\newpage

![Posterior quantiles of MSY, FMSY, and depletion in 1990 under each parameterization. These use the posteriors fit using the adjusted sampler parameters. The 95%, 80%, and 50% credible intervals are shown, along with the median. All parameterizations besides the *truncated* parameterization show very similar posterior distributions.](figs/mgt_quant.png){#fig:mgt_quant}

\newpage

![Posterior biomass series assuming different levels of catch error. The level of catch error does not appear to have much influence on the marginal distributions of biomass for each year.](figs/csd_biomass.png){#fig:csd_biomass}

\newpage

![Posterior distributions of MSY, FMSY, and depletion in 1990. Changes in assumed catch error do not appear to have a systematic influence on these posterior distributions.](figs/csd_mgt.png){#fig:csd_mgt}