For variance parameter $\sigma^2$, observed CPUE index in year $t$ $I_t$ (with $n$ total years), catchability $q$, and predicted biomass $B_t$, we have observation likelihood (up to a constant of proportionality):

$$\mathcal{L} \propto \frac{1}{\sigma^n}
  \exp\left\{
    -\frac{\sum_t \left( \log I_t - \log(qB_t) \right)^2}
          {2\sigma^2}
      \right\}$$

Let
$$Z_i = \frac{\log(I_t / B_t)}{n}$$
and
$$q' = \log(q),$$
    
With a uniform prior on $q'$, the posterior and likelihood are the same, and we can use the substitutions above to get
$$p \propto \frac{1}{\sigma^n}
  \exp\left\{ -\frac{\sum_t \left( Z_t - q' \right)^2}
    {2\sigma^2} \right\}$$
    
Now let
$$\hat{q}' \propto \frac{\sum Z_i}{n}.$$
so that
$$p \propto \frac{1}{\sigma^n}
  \exp\left\{ -\frac{\sum_t \left( (Z_t - \hat{q}') - (q' - \hat{q}') \right)^2}
    {2\sigma^2} \right\}.$$
    
Expanding the quadratic we get
$$p \propto \frac{1}{\sigma^n}
  \exp\left\{ -\frac{\sum_t \left( Z_t - \hat{q}' \right)^2 -
                             n \left(q' - \hat{q}' \right)^2}
    {2\sigma^2} \right\}.$$
    
Integting over $q'$ to marginalize it out,
$$p \propto \frac{1}{\sigma^n}
  \exp\left\{ -\frac{\sum_t \left( Z_t - \hat{q}' \right)^2}{2\sigma^2} \right\}
  \int_{-\infty}^\infty
     \exp \left\{
         -\frac{n}{2\sigma^2}
         \left(
             q' - \hat{q}'
         \right)^2
     \right\} \mathrm{d}q'.$$

The term in the integral is the kernel of a Normal distribution, and integrates to a constant, so that
$$p_{q'} \propto \frac{1}{\sigma^n}
  \exp\left\{ -\frac{\sum_t \left( Z_t - \hat{q}' \right)^2}
    {2\sigma^2} \right\},$$
showing that
$$Z_i \sim \operatorname{Normal}(\hat{q}', \sigma^2)$$

