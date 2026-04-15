import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.linalg import fractional_matrix_power


def get_max_eval_and_evec(xvec, yvec):
    vec = np.array([xvec, yvec]).T
    cov = 1 / (len(xvec) - 1) * vec.T @ vec
    eigs = np.linalg.eig(cov)
    max_idx = np.argmax(eigs.eigenvalues)
    min_idx = np.argmin(eigs.eigenvalues)

    return eigs.eigenvalues[max_idx], eigs.eigenvectors[:, max_idx]


def add_mahal(df):
    residx = df["residuals_x"][1:].to_numpy()
    residy = df["residuals_y"][1:].to_numpy()
    vec = np.array([residx, residy]).T

    mean = np.mean(vec, axis=0)
    centered = vec - mean
    cov = 1 / (len(residx) - 1) * (vec.T @ vec)

    mahals = []
    mahals.append(0)
    for x in vec:
        mahals.append(np.sqrt((x - mean) @ np.linalg.inv(cov) @ (x - mean).T))

    df["mahal"] = mahals
    return df


def add_zscores(df):
    xvec = df["residuals_x"][1:].to_numpy()
    yvec = df["residuals_y"][1:].to_numpy()

    vec = np.array([xvec, yvec]).T
    S = vec.T @ vec / (len(xvec) - 1)

    Sinvhalf = fractional_matrix_power(S, -0.5)

    normed_residuals_x = []
    normed_residuals_y = []
    normed_residuals_x.append(0)
    normed_residuals_y.append(0)
    for v in vec:
        res = Sinvhalf @ v
        normed_residuals_x.append(res[0])
        normed_residuals_y.append(res[1])

    print(
        "drift = ",
        np.sqrt(
            (np.sum(normed_residuals_x) / len(xvec)) ** 2
            + (np.sum(normed_residuals_y) / len(yvec)) ** 2
        ),
    )

    # x_std = np.std(xvec)
    # y_std = np.std(yvec)
    # # meanx = np.mean(residx)
    # # meany = np.mean(residy)
    # # meanx = np.mean(df["state_posts_x"].to_numpy())
    # # meany = np.mean(df["state_posts_y"].to_numpy())
    # meanx = 0
    # meany = 0

    # zscore_x = []
    # zscore_y = []

    # zscore_x.append(0)
    # zscore_y.append(0)
    # for x, y in zip(xvec, yvec):
    #     zscore_x.append((x - meanx) / x_std)
    #     zscore_y.append((y - meany) / y_std)

    df["zscore_x"] = normed_residuals_x
    df["zscore_y"] = normed_residuals_y
    return df


def add_residuals(df):
    # Add residuals
    residuals_x = []
    residuals_x.append(0)
    residuals_y = []
    residuals_y.append(0)
    for tx, px, ty, py in zip(
        df["true_pos_x"][1:],
        df["state_posts_x"][:-1],
        df["true_pos_y"][1:],
        df["state_posts_y"][:-1],
    ):
        residuals_x.append(tx - px)
        residuals_y.append(ty - py)

    df["residuals_x"] = residuals_x
    df["residuals_y"] = residuals_y
    return df


def plot(df, plot_name, max_evec_posts, max_evec_res):
    fig, ax = plt.subplots(3, 1)

    ax[0].scatter(df["true_pos_x"], df["true_pos_y"], label="True position")
    ax[0].scatter(df["state_posts_x"], df["state_posts_y"], label="KF posterior")
    # ax[0].annotate(
    #     "",
    #     xytext=(0, 0),
    #     xy=(max_evec_posts[0], max_evec_posts[1]),
    #     arrowprops=dict(arrowstyle="->"),
    # )
    ax[0].scatter(
        np.mean(df["state_posts_x"].to_numpy()),
        np.mean(df["state_posts_y"].to_numpy()),
        label="Posterior mean",
    )
    ax[0].set_xlim(-0.2, 0.2)
    ax[0].set_ylim(-1.5, 1.5)
    ax[0].legend()
    ax[0].grid()
    ax[0].locator_params(axis="x", nbins=5)
    ax[0].locator_params(axis="y", nbins=5)

    ax[1].scatter(df["residuals_x"], df["residuals_y"], label="residuals")
    # ax[1].annotate(
    #     "",
    #     xytext=(0, 0),
    #     xy=(max_evec_res[0], max_evec_res[1]),
    #     arrowprops=dict(arrowstyle="->"),
    # )
    meanx = np.mean(df["residuals_x"][1:].to_numpy())
    meany = np.mean(df["residuals_y"][1:].to_numpy())
    ax[1].scatter(meanx, meany, label="mean res")

    ax[1].set_xlim(-0.2, 0.2)
    ax[1].set_ylim(-2.2, 2.2)

    ax[1].legend()
    ax[1].grid()
    ax[1].locator_params(axis="x", nbins=5)
    ax[1].locator_params(axis="y", nbins=5)

    ax[2].scatter(df["zscore_x"], df["zscore_y"], label="zscores")
    ax[2].set_xlim(-2, 2)
    ax[2].set_ylim(-2, 2)

    ax[2].scatter(
        np.mean(df["zscore_x"].to_numpy()),
        np.mean(df["zscore_y"].to_numpy()),
        label="mean",
    )
    ax[2].annotate(
        "",
        xytext=(0, 0),
        xy=(np.mean(df["zscore_x"].to_numpy()), np.mean(df["zscore_y"].to_numpy())),
        arrowprops=dict(arrowstyle="->"),
    )
    ax[2].legend()
    ax[2].grid()
    ax[2].locator_params(axis="x", nbins=5)
    ax[2].locator_params(axis="y", nbins=5)

    plt.savefig(plot_name)
    plt.close()


def run(df, plot_name):
    df = add_residuals(df)
    df = add_mahal(df)
    df = add_zscores(df)
    # Eigenvalue/PCA decomposition of posterior states
    max_eval_posts, max_evec_posts = get_max_eval_and_evec(
        df["state_posts_x"][1:].to_numpy(), df["state_posts_y"][1:].to_numpy()
    )

    # Eigenvalue/PCA decomposition of residuals
    max_eval_res, max_evec_res = get_max_eval_and_evec(
        df["residuals_x"][1:].to_numpy(), df["residuals_y"][1:].to_numpy()
    )

    plot(df, plot_name, max_evec_posts, max_evec_res)


df_random_noise = {
    "true_pos_x": [
        0.05,
        -0.08,
        0.12,
        -0.04,
        0.09,
        -0.10,
        0.03,
        0.07,
        -0.06,
        0.11,
        -0.02,
        0.04,
        -0.09,
        0.08,
        -0.03,
        0.06,
        -0.07,
        0.02,
        -0.05,
        0.01,
    ],
    "true_pos_y": [
        -0.07,
        0.11,
        -0.13,
        0.06,
        -0.02,
        0.09,
        -0.05,
        0.03,
        -0.08,
        0.12,
        -0.01,
        -0.04,
        0.10,
        -0.06,
        0.07,
        -0.02,
        0.05,
        -0.09,
        0.04,
        -0.03,
    ],
    "state_posts_x": [
        0.05,
        -0.02,
        0.06,
        0.01,
        0.04,
        -0.03,
        0.00,
        0.03,
        -0.02,
        0.05,
        0.01,
        0.02,
        -0.04,
        0.03,
        -0.01,
        0.03,
        -0.03,
        0.01,
        -0.02,
        0.00,
    ],
    "state_posts_y": [
        -0.07,
        0.02,
        -0.05,
        0.01,
        -0.01,
        0.03,
        -0.01,
        0.01,
        -0.03,
        0.04,
        0.00,
        -0.02,
        0.03,
        -0.02,
        0.02,
        -0.01,
        0.01,
        -0.03,
        0.02,
        -0.01,
    ],
}

df_random_noise = pd.DataFrame(df_random_noise)
run(df_random_noise, "random_noise.png")

df_drift_smooth = {
    "true_pos_x": [0.1, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3],
    "true_pos_y": [0.05, 0.12, 0.18, 0.25, 0.32, 0.4, 0.48],
    "state_posts_x": [0.1, 0.27, 0.47, 0.66, 0.85, 1.03, 1.2],
    "state_posts_y": [0.05, 0.1, 0.16, 0.22, 0.29, 0.35, 0.42],
}
df_drift_smooth = pd.DataFrame(df_drift_smooth)
run(df_drift_smooth, "drift_smooth.png")


df_up_and_down = {
    "true_pos_x": [0.1, 0.3, 0.55, 0.8, 1.05, 1.26, 1.5],
    "true_pos_y": [0.02, 1, -0.2, 1.3, 0.6, 1.5, 0.9],
    "state_posts_x": [0.1, 0.27, 0.5, 0.76, 1, 1.2, 1.4],
    "state_posts_y": [0.02, 0.8, -0.0, 1.03, 0.75, 1.22, 0.99],
}

df_up_and_down = pd.DataFrame(df_up_and_down)
run(df_up_and_down, "drift_with_up_and_down.png")

df_high_var_y = {
    "true_pos_x": [
        0.02,
        -0.01,
        0.03,
        -0.02,
        0.01,
        -0.03,
        0.02,
        -0.01,
        0.01,
        -0.02,
        0.02,
        -0.01,
        0.03,
        -0.02,
        0.01,
        -0.03,
        0.02,
        -0.01,
        0.01,
        -0.02,
    ],
    "true_pos_y": [
        0.9,
        -1.1,
        1.2,
        -0.8,
        1.0,
        -1.3,
        0.7,
        -1.0,
        1.1,
        -0.9,
        0.8,
        -1.2,
        1.3,
        -0.7,
        0.9,
        -1.1,
        1.0,
        -0.8,
        1.2,
        -1.0,
    ],
    "state_posts_x": [
        0.02,
        0.00,
        0.02,
        -0.01,
        0.00,
        -0.01,
        0.01,
        0.00,
        0.00,
        -0.01,
        0.01,
        0.00,
        0.02,
        -0.01,
        0.00,
        -0.01,
        0.01,
        0.00,
        0.00,
        -0.01,
    ],
    "state_posts_y": [
        0.5,
        -0.6,
        0.7,
        -0.4,
        0.6,
        -0.8,
        0.4,
        -0.6,
        0.7,
        -0.5,
        0.5,
        -0.7,
        0.8,
        -0.4,
        0.6,
        -0.6,
        0.6,
        -0.5,
        0.7,
        -0.6,
    ],
}
run(pd.DataFrame(df_high_var_y), "high_variance_y.png")
