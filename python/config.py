from pathlib import Path
from os import path
from os import path

file_path = Path(__file__).resolve()
ROOT_PATH = str(file_path.parents[1])
DATA_PATH = path.join(ROOT_PATH, 'data')

sprat_data_path = path.join(DATA_PATH, 'project', 'VPA_assignment_sprat.xlsx')