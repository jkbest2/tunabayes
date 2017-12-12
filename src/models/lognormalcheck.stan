generated quantities {
    real X;
    real Xmode;
    real Xmean;

    X = lognormal_rng(log(100), 0.5);
    Xmode = lognormal_rng(log(100) + 0.5^2, 0.5);
    Xmean = lognormal_rng(log(100) - 0.5^2 / 2, 0.5);
}

