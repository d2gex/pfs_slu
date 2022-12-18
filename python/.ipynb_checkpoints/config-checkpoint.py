from pathlib import Path
from os import path
file_path = Path(__file__).resolve()
ROOT_PATH = str(file_path.parents[1])
DATA_PATH = path.join(ROOT_PATH, 'data')