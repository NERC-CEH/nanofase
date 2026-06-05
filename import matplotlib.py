import matplotlib.pyplot as plt
import pandas as pd

df_new = pd.read_csv("/users/pollution/canulu/Desktop/nanofase/output/output_water_new.csv", comment='#')
# if you also have an old run:
df_old = pd.read_csv("/users/pollution/canulu/nanofase1/output/output_water.csv", comment='#')

df_new["datetime"] = pd.to_datetime(df_new["datetime"])
df_old["datetime"] = pd.to_datetime(df_old["datetime"])

import matplotlib.pyplot as plt

# unique coordinates (sorted so the grid is structured nicely)
norths = sorted(df_new["norths"].unique(), reverse=True)  # rows (top = largest northing)
easts  = sorted(df_new["easts"].unique())                 # columns

nrows = len(norths)
ncols = len(easts)

fig, axes = plt.subplots(nrows=nrows, ncols=ncols,
                         figsize=(14, 8), sharex=True, sharey=True)

for i, north in enumerate(norths):
    for j, east in enumerate(easts):
        ax = axes[i, j]

        # NEW model
        sub_new = df_new[(df_new["norths"] == north) & (df_new["easts"] == east)] \
                    .sort_values("datetime")
        ax.plot(sub_new["datetime"], sub_new["C_spm(kg/m3)"],
                label="FASE (new)")

        # OLD model (uncomment if you have df_old)
        sub_old = df_old[(df_old["norths"] == north) & (df_old["easts"] == east)] \
                     .sort_values("datetime")
        ax.plot(sub_old["datetime"], sub_old["C_spm(kg/m3)"],
                 label="NanoFASE (old)")

        # title as (norths, easts) like in your example
        ax.set_title(f"location = ({int(north)}, {int(east)})", fontsize=9)

        if i == nrows - 1:
            ax.set_xlabel("t")
        if j == 0:
            ax.set_ylabel("SPM conc")

        # only show legend once
        if i == 0 and j == 1:
            ax.legend()

fig.autofmt_xdate(rotation=45)
plt.tight_layout()
plt.show()

