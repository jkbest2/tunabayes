using Distributions
using Plots; gr()
using StatPlots

function C_pdf(log_μ::T, log_q::T, log = false) where {T}
    σ = T(1)
    log_C = log(0.2)
    q = exp(log_q)
    p = 1 / q * pdf(LogNormal(μ, σ), exp(log_q + log_C))
    if 
end

μ = log.(linspace(1e-3, 10, 1024))'
q = linspace(1e-3, 10, 1024)

p = log.(C_pdf.(μ, q))

nll_fn = OnceDifferentiable(θ -> -log(C_pdf(θ[1], θ[2])), ones(2), autodiff = :forward)

opt = optimize(nll_fn, ones(2))
