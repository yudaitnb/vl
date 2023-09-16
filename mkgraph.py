import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
import matplotlib.cm as cm
mpl.use('Agg')
import pandas as pd
# set up the figure and axes
fig = plt.figure(figsize=(8, 8))
ax1 = fig.add_subplot(121, projection='3d')
ax2 = fig.add_subplot(122, projection='3d')

# import data
df = pd.read_csv('output.csv', index_col=0)

# Group by and compute mean
stats = df.groupby(['nmod', 'nver'])['ConsRes_Real'].mean().reset_index()

# Create a unique color for each (nmod, nver) pair
unique_pairs = [tuple(x) for x in stats[['nmod', 'nver']].drop_duplicates().values]
colors = cm.gray(np.linspace(0.25, 1, len(unique_pairs)))
color_mapping = {(nmod, nver): color for (nmod, nver), color in zip(unique_pairs, colors)}

stats['color'] = stats.apply(lambda row: color_mapping[(row['nmod'], row['nver'])], axis=1)

# Plot
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

# Adjust these values to make the bars narrower
dx = 0.5
dy = 0.5
dz = stats['ConsRes_Real']

x = stats['nmod'] - dx/2
y = stats['nver'] - dy/2
z = np.zeros_like(x)

# Create bars with the designated colors
bars = ax.bar3d(x, y, z, dx, dy, dz, shade=True, color=stats['color'],edgecolor='black')

# ax.set_xlabel('Number of Modules')
# ax.set_ylabel('Number of Versions\n per Module')
ax.set_xlabel('#mod')
ax.set_ylabel('#ver')
ax.set_zlabel('Resolution Time (s)')
ax.view_init(azim=225)

# Set y-axis labels to two lines
ax.set_yticks(np.unique(stats['nver']))

# Display the color map legend
from matplotlib.patches import Patch
legend_elements = [Patch(facecolor=color_mapping[pair], edgecolor='black', label=f'nmod={pair[0]}, nver={pair[1]}') for pair in unique_pairs]

for (x, y, z) in zip(stats['nmod'], stats['nver'], dz):
    ax.text(x, y, z, f"{z:.2f}", ha='center', va='bottom', fontsize=10,
    bbox=dict(boxstyle="square,pad=0.05", facecolor='white', edgecolor='none', alpha=0.8))

# plt.show()
# plt.tight_layout()
# plt.figure(figsize=(5, 5))  # 6x4インチの図を生成
plt.subplots_adjust(left=-0.05, right=1, top=1.07, bottom=0)

plt.savefig('./ret.png')