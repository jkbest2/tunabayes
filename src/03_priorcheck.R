priorfit <- stan('priorcheck.stan', algorithm = "Fixed_param")
monitor(priorfit, probs = c(0.1, 0.9), digits_summary = 2)
