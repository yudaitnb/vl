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

# # Step 2: Group by and compute mean
# grouped = df.groupby(['nmod', 'nver'])['ConsRes_Real'].mean().reset_index()



# # Step 3: Plot
# fig = plt.figure()
# ax = fig.add_subplot(111, projection='3d')

# x = grouped['nmod']
# y = grouped['nver']
# z = np.zeros_like(x)
# dx = np.ones_like(x)
# dy = np.ones_like(y)
# dz = grouped['ConsRes_Real']

# ax.bar3d(x, y, z, dx, dy, dz, shade=False, color=(0.8, 0.8, 0.8, 0.9), edgecolor='black')

# ax.set_xlabel('nmod')
# ax.set_ylabel('nver')
# ax.set_zlabel('Time of Constrain Resolution (s)')

# ax.set_yticks(np.unique(grouped['nver']))


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

x = stats['nmod']
y = stats['nver']
z = np.zeros_like(x)

# Adjust these values to make the bars narrower
dx = 0.75
dy = 0.5
dz = stats['ConsRes_Real']

# Create bars with the designated colors
bars = ax.bar3d(x, y, z, dx, dy, dz, shade=True, color=stats['color'],edgecolor='black')

ax.set_xlabel('Number of Modules')
ax.set_ylabel('Number of Versions\n per Module')
ax.set_zlabel('Resolution Time (s)')

# Set y-axis labels to two lines
# unique_nver = np.unique(stats['nver'])
ax.set_yticks(np.unique(stats['nver']))
# new_labels = [f"Line1\n{int(val)}" for val in unique_nver]  # Adjust "Line1" as needed
# ax.set_yticks(unique_nver)
# ax.set_yticklabels(new_labels, verticalalignment='center')

# Display the color map legend
from matplotlib.patches import Patch
legend_elements = [Patch(facecolor=color_mapping[pair], edgecolor='black', label=f'nmod={pair[0]}, nver={pair[1]}') for pair in unique_pairs]
# ax.legend(handles=legend_elements, loc='upper left')

for (x, y, z) in zip(stats['nmod'], stats['nver'], dz):
    # ax.text(x + 0.63 * dx, y + 0.3 * dy, z + 1, f"{round(z)}", ha='center', va='bottom', fontsize=10,
    # bbox=dict(boxstyle="square,pad=0.05", facecolor='white', edgecolor='none', alpha=0.6))
    ax.text(x + 0.63 * dx, y + 0.3 * dy, z + 1, f"{z:.2f}", ha='center', va='bottom', fontsize=10,
    bbox=dict(boxstyle="square,pad=0.05", facecolor='white', edgecolor='none', alpha=0.6))

# plt.show()
# plt.tight_layout()
# plt.figure(figsize=(5, 5))  # 6x4インチの図を生成
plt.subplots_adjust(left=-0.05, right=1, top=1.07, bottom=0)

plt.savefig('./ret.png')