#!/usr/bin/env python3
"""Post 5 energtx Batch 5 infographics (absolute energy scale) to Bluesky."""

import re
import time
from pathlib import Path
from atproto import Client, models

# -- Load credentials --
env_path = Path(__file__).parent / ".env.bsky"
env = {}
with open(env_path) as f:
    for line in f:
        line = line.strip()
        if "=" in line and not line.startswith("#"):
            k, v = line.split("=", 1)
            env[k.strip()] = v.strip()

handle = env.get("BSKY_HANDLE", "")
password = env.get("BSKY_APP_PASSWORD", "")

if not password or password == "buraya-app-password-yazin":
    print("ERROR: Set BSKY_APP_PASSWORD in .env.bsky")
    exit(1)


def build_facets(text: str):
    """Make each #hashtag and URL a clickable facet."""
    facets = []
    for m in re.finditer(r"#(\w+)", text):
        facets.append(models.AppBskyRichtextFacet.Main(
            index=models.AppBskyRichtextFacet.ByteSlice(
                byte_start=len(text[:m.start()].encode("utf-8")),
                byte_end=len(text[:m.end()].encode("utf-8"))),
            features=[models.AppBskyRichtextFacet.Tag(tag=m.group(1))],
        ))
    for m in re.finditer(r"https?://[^\s]+", text):
        facets.append(models.AppBskyRichtextFacet.Main(
            index=models.AppBskyRichtextFacet.ByteSlice(
                byte_start=len(text[:m.start()].encode("utf-8")),
                byte_end=len(text[:m.end()].encode("utf-8"))),
            features=[models.AppBskyRichtextFacet.Link(uri=m.group(0))],
        ))
    return facets


# -- Posts (text mirrors 03_post_texts.md 166-170) --
posts = [
    {
        "file": "166_largest_energy_consumers.png",
        "text": ("Who actually burns the most energy? \U0001F525\n"
                 "Total primary energy consumption (TWh), 2023.\n\n"
                 "Forget per-capita for a second — in raw scale, China and the US dwarf everyone. India is now a clear third.\n\n"
                 "Explore → energtx.com/datasets\n\n"
                 "#Energy #EnergyData #China #DataViz #RStats"),
        "alt": "Bar chart: world's 15 largest primary energy consumers (TWh), 2023. China and the US lead by a wide margin.",
    },
    {
        "file": "167_total_energy_china_overtakes.png",
        "text": ("The crossover that reshaped the world ⚡\n"
                 "Total energy consumption (TWh), 1990 → 2023.\n\n"
                 "China passed the US around 2009 and never looked back. Japan, Germany & Russia stayed flat or fell.\n\n"
                 "Explore → energtx.com/datasets\n\n"
                 "#Energy #China #EnergyTransition #DataViz #RStats"),
        "alt": "Line chart: total primary energy consumption (TWh), 1990-2023, for China, US, India, Russia, Japan, Germany. China's line overtakes the US around 2009.",
    },
    {
        "file": "168_energy_demand_change.png",
        "text": ("Where energy demand exploded — and where it shrank \U0001F4CA\n"
                 "Change in total energy use (TWh), 2000 → 2023.\n\n"
                 "Teal = grew (China, India, the Gulf). Coral = shrank (UK, Japan, Italy). Two very different worlds.\n\n"
                 "Explore → energtx.com/datasets\n\n"
                 "#Energy #EnergyData #China #DataViz #RStats"),
        "alt": "Diverging bar chart: change in total primary energy consumption (TWh), 2000 vs 2023. China and India grew most; Japan, Germany, UK and Italy declined.",
    },
    {
        "file": "169_largest_electricity_producers.png",
        "text": ("Who generates the world's electricity? \U0001F50C\n"
                 "Total electricity generation (TWh), 2023.\n\n"
                 "China now produces roughly twice the US — and more than the next several countries combined.\n\n"
                 "Explore → energtx.com/datasets\n\n"
                 "#Electricity #Energy #EnergyData #DataViz #RStats"),
        "alt": "Bar chart: world's 15 biggest electricity producers by total generation (TWh), 2023. China leads with roughly double the US.",
    },
    {
        "file": "170_energy_vs_economy_scatter.png",
        "text": ("Bigger economy, more energy — usually \U0001F4B0⚡\n"
                 "Energy use vs GDP, 2022 (bubble = population).\n\n"
                 "The link is strong, but not destiny: some nations get far more GDP per unit of energy than others.\n\n"
                 "Explore → energtx.com/datasets\n\n"
                 "#Energy #Economy #EnergyData #DataViz #RStats"),
        "alt": "Scatter plot: primary energy consumption (TWh) vs GDP for major economies, 2022, bubble size = population. China and the US are outliers.",
    },
]

# -- Login --
client = Client()
client.login(handle, password)
print(f"✓ Logged in as {handle}")

png_dir = Path(__file__).parent / "png"
n = len(posts)

for i, post in enumerate(posts, 1):
    img_path = png_dir / post["file"]
    if not img_path.exists():
        print(f"✗ Missing: {post['file']}")
        continue
    try:
        with open(img_path, "rb") as f:
            img_data = f.read()
        upload = client.upload_blob(img_data)
        embed = models.AppBskyEmbedImages.Main(images=[
            models.AppBskyEmbedImages.Image(alt=post["alt"], image=upload.blob)
        ])
        client.send_post(text=post["text"], embed=embed,
                         facets=build_facets(post["text"]))
        print(f"✓ Posted {i}/{n}: {post['file']}")
    except Exception as e:
        print(f"✗ Error posting {post['file']}: {e}")
    if i < n:
        time.sleep(5)

print(f"\n✅ Done! {n} posts published.")
