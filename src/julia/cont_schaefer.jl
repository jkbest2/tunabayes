using DifferentialEquations
using Distributions
using TransformVariables, LogDensityProblems, DynamicHMC, MCMCDiagnostics,
    Parameters, Statistics, Distributions, ForwardDiff

T = 10
F = rand(LogNormal(log(0.15), 0.5), T)

P = zeros(T)
C = zeros(T)

module ContinuousSchaefer
using DifferentialEquations
using Distributions
using TransformVariables, LogDensityProblems, DynamicHMC, MCMCDiagnostics,
    Parameters, Statistics, Distributions, ForwardDiff

export popdy!, pop_steps, FishProblem

function popdy!(du, u, θ, t)
    r, F = θ
    du[1] = r * u[1] * (1 - u[1]) - F * u[1]
    du[2] = F * u[1]
end

function pop_steps(r, P, F)
    T = length(F)
    P_med = zeros(T + 1)           # Median population prediction
    C_pred = zeros(T)                   # Predicted catch

    tspan = (0.0, 1.0)

    P_med[1] = 1.0                 # Start at carrying capacity
    for t in 1:T
        prob = ODEProblem(popdy!,
                          [P[t], 0.0],
                          tspan,
                          (r, F[t]))
        sol = solve(prob, saveat = 1.0)
        P_med[t + 1], C_pred[t] = sol[end]
    end

    P_med, C_pred
end

struct FishProblem{Ti<:Integer, Tv<:AbstractVector, Tf<:Real}
    T::Ti
    I_obs::Tv
    C_obs::Tv
    catch_sd::Tf
end

function (fishprob::FishProblem)(θ)
    @unpack T, I_obs, C_obs, catch_sd = fishprob
    @unpack r, K, q, σ², τ², P, F = θ

    σ = √σ²
    τ = √τ²

    # lp = zeros(typeof(r))
    tspan = (0.0, 1.0)

    P_med, C_pred = pop_steps(r, P, F)

    ## Priors from Meyer and Millar
    lp_1 = logpdf(LogNormal(-1.38, 1 / sqrt(3.845)), r)
    lp_1 += logpdf(LogNormal(5.042905, 1 / sqrt(3.7603664)), K)
    lp_1 += -log(q)
    lp_1 += logpdf(InverseGamma(3.785518, 0.010223), σ²)
    lp_1 += logpdf(InverseGamma(1.708603, 0.008613854), τ²)

    ## Prior on depletion
    lp_2 = sum(logpdf.(LogNormal.(log.(P_med[1:T]), σ), P))

    ## CPUE index likelihood
    lp_3 = sum(logpdf.(LogNormal.(log.(q .* K .* P), τ), I_obs))

    ## Observed catch likelhood
    lp_4 = sum(logpdf.(Normal.(C_pred .* K, catch_sd), C_obs))

    lp_1 + lp_2 + lp_3 + lp_4
end
end # module

## Mean from posterior of short run in Stan
r = 0.299
K = 2.752e2
q = 0.242
σ² = 3.116801e-03
τ² = 1.228889e-02

P = [1.020600e+00, 9.984886e-01, 8.824470e-01, 7.873076e-01, 7.548347e-01,
     6.826705e-01, 6.110864e-01, 5.850155e-01, 5.963007e-01, 5.957245e-01,
     5.933224e-01, 5.875418e-01, 5.676554e-01, 5.587779e-01, 5.395169e-01,
     5.165701e-01, 4.746077e-01, 5.031790e-01, 5.150696e-01, 4.797403e-01,
     4.191102e-01, 3.554354e-01, 3.295867e-01]

F = [2.843152e+00, 2.344898e+00, 2.152315e+00, 2.250601e+00, 2.161642e+00,
     1.795083e+00, 1.870025e+00, 2.192880e+00, 2.325929e+00, 2.228507e+00,
     2.112157e+00, 2.036211e+00, 2.033585e+00, 2.019325e+00, 1.940855e+00,
     1.694749e+00, 2.327205e+00, 2.460987e+00, 1.715932e+00, 1.456825e+00,
     1.257301e+00, 1.477608e+00, 1.429273e+00]
Frate = exp.(-F)

P_med, C_pred = pop_steps(r, P, Frate)

T = 23
C_obs = [15.9, 25.7, 28.5, 23.7, 25.0, 33.3, 28.2, 19.7, 17.5, 19.3, 21.6, 23.1,
         22.5, 22.5, 23.6, 29.1, 14.4, 13.2, 28.4, 34.6, 37.5, 25.9, 25.3]

I_obs = [61.89, 78.98, 55.59, 44.61, 56.89, 38.27, 33.84, 36.13, 41.95, 36.63,
         36.33, 38.82, 34.32, 37.64, 34.01, 32.16, 26.88, 36.61, 30.07, 30.75,
         23.36, 22.36, 21.91]
catch_sd = 0.2

fishprob = FishProblem(T, I_obs, C_obs, catch_sd)

fishprob((r = r, K = K, q = q, σ² = σ², τ² = τ², P = P, F = Frate))

problem_transformation(p::FishProblem) =
    as((r = asℝ₊, K = asℝ₊, q = asℝ₊,
       σ² = asℝ₊, τ² = asℝ₊,
       P = as(Array, asℝ₊, p.T),
       F = as(Array, asℝ₊, p.T)))

P = TransformedLogDensity(problem_transformation(fishprob), fishprob)

∇P = ADgradient(:ForwardDiff, P)

chain, NUTS_tuned = NUTS_init_tune_mcmc(∇P, 5000)

##--Continuous time solution----------------------------------------------------
E = C_obs ./ I_obs
E2 = [E; 0.0]


function popdy2!(du, u, θ, t)
    r, q, E = θ
    E_idx = floor(Int, t)
    du[1] = r * u[1] * (1 - u[1]) - q * E[E_idx] * u[1]
    du[2] = q * E[E_idx] * u[1]
end

prob = ODEProblem(popdy2!,
                  [1.0, 0.0],
                  (1.0, 24.0),
                  (r, q, [E; 0.0]))

sol = solve(prob,
            saveat = 1.0:24.0,
            tstops = 1.0:24.0)

function f_obj(θ, dat)
    P0, r, K, q = θ
    E, C = dat
    prob = ODEProblem(popdy2!,
                      [P0, 0.0],
                      (1.0, 24.0),
                      (r, q, [E; 0.0]))
    sol = solve(prob,
                saveat = 1.0:24.0,
                tstops = 1.0:24.0)
    sum((C .- diff(sol[2, :]) * K).^2)
end

using Optim

θ_init = [1.02, 0.3, 220, 0.28]
opt = optimize(θ -> f_obj(θ, (E2, C_obs)),
               θ_init,
               NelderMead())

θ_hat = Optim.minimizer(opt)
P0, r, K, q = θ_hat

prob = ODEProblem(popdy2!,
                  [P0, 0.0],
                  (1.0, 24.0),
                  (r, q, [E; 0.0]))
sol = solve(prob,
            saveat = 1.0:24.0,
            tstops = 1.0:24.0)

C_est = diff(sol[2, :]) * K
