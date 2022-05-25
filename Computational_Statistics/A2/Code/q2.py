import numpy as np

dat = [-0.71, -1.30, -0.13, -2.03, 1.62, 2.38, 0.48, 0.51, -0.69, -2.32, -2.02,
       1.23, -0.25, 0.76, 0.65]

def my_func(x):
    return(np.median(np.cos(x)))

def draw_bs_sample(data):
    """Draw a bootstrap sample from a 1D data set."""
    return np.random.choice(data, size=len(data))

def studentized_bootstrap_ci(
    data,
    statfunction=np.mean,
    n_bs_samples=500,
    n_inner_samples=100,
    ptiles=(2.5, 97.5),
):
    thetas = np.empty(n_bs_samples)
    ts = np.zeros(n_bs_samples)

    # Outer sample
    for i in range(n_bs_samples):
        sample = draw_bs_sample(data)
        theta = statfunction(sample)
        thetas[i] = theta

        # Inner sample
        resample_stats = np.empty(n_inner_samples)
        for j in range(n_inner_samples):
            resample = draw_bs_sample(sample)
            resample_stats[j] = statfunction(resample)

        # Compute statistic of interest
        ts[i] = np.sum(resample_stats - theta) / np.std(resample_stats)

    s = np.std(thetas)
    ts = np.sort(ts)

    p1, p2 = np.percentile(s*ts, ptiles)
    ci_high = np.mean(data)-(s*p1)
    ci_low = np.mean(data)-(s*p2)
    return (ci_low, ci_high)

studentized_ctrl_ci = studentized_bootstrap_ci(dat, statfunction=my_func)
studentized_pest_ci = studentized_bootstrap_ci(dat,statfunction=my_func)

print(f"Control Alive Sperm Millions 95% C.I.: {studentized_ctrl_ci}")
print(f"Pesticide Alive Sperm Millions 95% C.I.: {studentized_pest_ci}")