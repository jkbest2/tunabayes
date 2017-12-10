fitmod <- function(file, data, chains, iter, ...) {
    mod <- stan_model(file)
    start <- Sys.time()
    fit <- sampling(mod, data = data,
                    chains = chains, iter = iter, ...)
    end <- Sys.time()

    list(fit = fit, time = end - start)
}

