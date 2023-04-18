import os
import time

progress = os.listdir("all_codes")
progress = len(progress)
total = 3131
print(progress/total)

while progress < total:
    progress = os.listdir("all_codes")
    progress = len(progress)
    total = 3131
    print(progress/total)
    time.sleep(1)


import os
import time
from tqdm import tqdm

progress = os.listdir("all_codes")
progress = len(progress)
total = 3131
print(progress/total)
    
with tqdm(total = total) as pbar:  
  while progress < total:
    progress = os.listdir("all_codes")
    progress = len(progress)
    time.sleep(1)
