import numpy as np
import pandas as pd
from PIL import Image

########## PARAMETERS ##########
IMAGE_NUM = 2
DISTANCE = "euclidean"
######## END PARAMETERS ########

df = pd.read_csv(f'images\\{IMAGE_NUM}\\shuffled.txt', sep=' ', header=None, index_col=False)
df.drop(df.columns[len(df.columns)-1], axis=1, inplace=True)

row_order = pd.read_csv(f'images\\{IMAGE_NUM}\\{DISTANCE}\\row_order.csv')

for iter in range(3):
    ordered_df = df.reindex(row_order.iloc[:,iter])
    restored_image = Image.fromarray(ordered_df.to_numpy().astype(np.uint8))
    restored_image.save(f'images\\{IMAGE_NUM}\\{DISTANCE}\\nice_{ordered_df.index.name[1:]}_{DISTANCE}.png')