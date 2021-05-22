import numpy as np
import matplotlib.pyplot as plt
import matplotlib

dpi = 96

# Distributions
def plot_dist(name, xs, pdf):
    fig = plt.figure(figsize=(320/dpi, 240/dpi), dpi=dpi)
    ax = fig.add_subplot()

    line, = ax.plot(xs, pdf, color='k') # k = black
    line.set_antialiased(True)

    for side in ['left', 'top', 'right']:
        ax.spines[side].set_visible(False)

    ax.set_yticks([])
    ax.set_xticks([0])

    # give the 0 a slight nudge to the left as the default is a bit skewed towards right
    offset = matplotlib.transforms.ScaledTranslation(-1/256., 0., fig.dpi_scale_trans)
    for label in ax.xaxis.get_majorticklabels():
        label.set_transform(label.get_transform() + offset)

    fig.tight_layout()
    plt.savefig(f'{name}.png', transparent=True)

loc, scale = 0., 1.

x = np.arange(-5, 5, 0.01)
plot_dist('laplace', x, np.exp(-abs(x-loc)/scale)/(2.*scale))
plot_dist('normal', x, 1/(scale * np.sqrt(2 * np.pi)) * np.exp(- 1/2 * (x - loc)**2 / scale**2))


# Lasso & Ridge feasible solutions,
# reproduction of Figure 6.7 from https://www.statlearning.com/
def plot_feasible():
    fig, ax = plt.subplots(nrows=1, ncols=2, figsize=(640/dpi, 320/dpi), dpi=dpi)
    r = 0.25 # circle radius and square diagonal
    contours = [[0.8, 1.22, 1.55], [0.8, 1.22, 1.57]]
    beta_estimate = [(0.36, 0.65), (0.42, 0.65)] # I tried to match the original paper here

    for i, axis in enumerate(ax):
        axis.set_aspect('equal')

        axis.set_xlabel('$\\beta_1$')
        axis.set_ylabel('$\\beta_2$', rotation=0)
        axis.scatter(beta_estimate[i][0], beta_estimate[i][1], color='k', lw=0.1)
        axis.annotate("$\\hat\\beta$", (beta_estimate[i][0]+0.03, beta_estimate[i][1]+0.01))

        axis.set_xlim([-r-0.1,0.8])
        axis.set_ylim([-r-0.1,0.8])

        axis.grid(True, which='both', lw=0.8, color=(.4,.4,.4))

        axis.set_yticks([0])
        axis.set_xticks([0])

        for side in ['top', 'right']:
            axis.spines[side].set_visible(False)

        # give the 0 a slight nudge to the left as the default is a bit skewed towards right
        offset = matplotlib.transforms.ScaledTranslation(-1/256., 0., fig.dpi_scale_trans)
        for label in axis.xaxis.get_majorticklabels():
            label.set_transform(label.get_transform() + offset)

        for l in contours[i]:
            axis.add_artist(matplotlib.patches.Ellipse(
                xy=beta_estimate[i], width=l*0.25, height=l*0.7, angle=135, edgecolor=(1, 0, 0), fill=False))

    # Lasso
    s = np.sqrt(2) * r # diamond side = sqrt(r^2 + r^2)
    y_offset = -s * np.sqrt(2)/2 # move down by half a diagonal, Pythagoras again
    ax[0].add_artist(matplotlib.patches.Rectangle((0, y_offset), height=s, width=s, angle=45, color=(0,0,0,0.2)))

    # Ridge
    ax[1].add_artist(plt.Circle((0, 0), r, color=(0,0,0,0.2)))

    #
    fig.tight_layout()
    plt.savefig(f'feasible.png', transparent=True)

plot_feasible()
