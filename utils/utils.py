import os
import re
from pathlib import Path

import requests


def setup(y, d):
    p = Path(str(y)) / "inputs" / f"day{d}.txt"
    p.parent.mkdir(parents=True, exist_ok=True)
    session = os.getenv("session")
    if not p.exists():
        url = f"https://adventofcode.com/{y}/day/{d}/input"
        headers = {"Cookie": f"session={session}"}
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        with open(p, "w") as f:
            f.write(response.text)
    url = f"https://adventofcode.com/{y}/day/{d}"
    response = requests.get(url)
    response.raise_for_status()
    title = re.findall(r"<h2>--- Day .*: (.*) ---</h2>", response.text)[0].lower().replace(" ", "_")
    p2 = Path(str(y)) / f"day{d:02}_{title}.py"
    if not p2.exists():
        p2.touch()
    os.system(f"open {url}")
    os.system(f"code {p2}")
