generated quantities {
  real X;
  real Xmode;
  real Xmean;
  real u;
  real Y;

  X = lognormal_rng(log(100), 0.5);
  Xmode = lognormal_rng(log(100) + 0.5^2, 0.5);
  Xmean = lognormal_rng(log(100) - 0.5^2 / 2, 0.5);

  u = normal_rng(0, 1);
  Y = 100 * exp(u * 0.5);
}

