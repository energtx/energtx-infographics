"""energtx — Batch 3 Bluesky Poster (101-130)"""
import os, time, sys
from pathlib import Path
from atproto import Client, models

ENV_FILE = Path(__file__).parent / ".env.bsky"
PNG_DIR = Path(__file__).parent / "png"

env = {}
for line in ENV_FILE.read_text().strip().splitlines():
    if "=" in line and not line.startswith("#"):
        k, v = line.split("=", 1)
        env[k.strip()] = v.strip()

HANDLE = env.get("BLUESKY_HANDLE", "")
PASSWORD = env.get("BLUESKY_APP_PASSWORD", "")
if not PASSWORD:
    print("ERROR: BLUESKY_APP_PASSWORD is empty"); sys.exit(1)

POSTS = {
    101: "What powers the world? Oil 31.5%, Coal 26.2%, Gas 23.6%. Renewables still under 15%. The donut doesn't lie.\nenergтx.com/datasets | info@energtx.com\n#Energy #GlobalEnergy #Infographic #DataViz #Climate #NetZero #Sustainability",
    102: "What powers China? Coal still dominates at 55%+. But solar and wind are the fastest-growing slices.\nenergтx.com/datasets | info@energtx.com\n#China #Energy #Coal #Solar #Infographic #DataViz #ClimateAction",
    103: "What powers India? Coal at 55%, oil at 27%. Solar is growing but from a tiny base. 1.4 billion people, one energy challenge.\nenergтx.com/datasets | info@energtx.com\n#India #Energy #Coal #Infographic #DataViz #ClimateAction #Sustainability",
    104: "How renewable is each continent? A ridgeline plot showing the distribution. Americas and Europe spread wide. Asia clusters low.\nenergтx.com/datasets | info@energtx.com\n#Renewables #Continents #RidgePlot #DataViz #Climate #Sustainability #Infographic",
    105: "Energy per capita by continent: a ridgeline view. The inequality is staggering. Some clusters at 5,000 kWh, others at 80,000.\nenergтx.com/datasets | info@energtx.com\n#EnergyInequality #PerCapita #RidgePlot #DataViz #Infographic #GlobalEnergy",
    106: "Renewable energy: where were we in 2010, where are we now? A slope chart showing 10 countries. Every line goes up.\nenergтx.com/datasets | info@energtx.com\n#Renewables #EnergyTransition #SlopeChart #DataViz #Infographic #Climate #CleanEnergy",
    107: "Fossil fuel dependency: then vs now. A slope chart. Most lines go down, but some barely moved. The transition is uneven.\nenergтx.com/datasets | info@energtx.com\n#FossilFuels #EnergyTransition #SlopeChart #DataViz #Infographic #Climate",
    108: "Grid decarbonization: winners and losers. UK and Denmark improved dramatically. Others got worse. A diverging bar chart.\nenergтx.com/datasets | info@energtx.com\n#Decarbonization #CarbonIntensity #Electricity #DataViz #Infographic #Climate #NetZero",
    109: "G7 energy transitions: seven countries, seven stories. Faceted area charts reveal how differently each nation is evolving.\nenergтx.com/datasets | info@energtx.com\n#G7 #EnergyMix #SmallMultiples #DataViz #Infographic #Climate #EnergyTransition",
    110: "BRICS energy: five economies, five transformations. Brazil = green. China = coal+solar. Russia = gas. India = coal. SA = coal.\nenergтx.com/datasets | info@energtx.com\n#BRICS #Energy #SmallMultiples #DataViz #Infographic #GlobalEconomy #Sustainability",
    111: "Who gained renewable share since 2020? Who lost? A diverging bar chart reveals the fastest movers.\nenergтx.com/datasets | info@energtx.com\n#Renewables #CleanEnergy #DataViz #Infographic #Climate #EnergyTransition #Sustainability",
    112: "Solar power: 12 countries, 12 growth curves. Small multiples show exponential adoption everywhere. China's scale is unmatched.\nenergтx.com/datasets | info@energtx.com\n#Solar #SolarEnergy #SmallMultiples #DataViz #Infographic #CleanEnergy #Sustainability",
    113: "Wind power: 12 countries, 12 growth curves. Each nation found its own pace. Denmark leads in share, China in volume.\nenergтx.com/datasets | info@energtx.com\n#Wind #WindEnergy #SmallMultiples #DataViz #Infographic #CleanEnergy #Sustainability",
    114: "Who owns the solar market? China's share keeps growing. Everyone else is shrinking proportionally.\nenergтx.com/datasets | info@energtx.com\n#Solar #MarketShare #China #DataViz #Infographic #CleanEnergy #EnergyTransition",
    115: "Who owns the wind market? China surpassed the US around 2015 and never looked back.\nenergтx.com/datasets | info@energtx.com\n#Wind #MarketShare #China #US #DataViz #Infographic #CleanEnergy #EnergyTransition",
    116: "G7 clean energy scorecard: a radar chart comparing renewables, nuclear, solar, wind, and low-carbon electricity.\nenergтx.com/datasets | info@energtx.com\n#G7 #CleanEnergy #RadarChart #DataViz #Infographic #Climate #Sustainability",
    117: "What powers the United States? Gas 40%, oil 36%, coal 10%. Nuclear 8%. Renewables just 12%.\nenergтx.com/datasets | info@energtx.com\n#US #USA #Energy #DonutChart #DataViz #Infographic #Climate",
    118: "What powers Germany? Oil 33%, gas 24%, coal 16%. Renewables at 24% and climbing fast.\nenergтx.com/datasets | info@energtx.com\n#Germany #Energiewende #DonutChart #DataViz #Infographic #Climate #EnergyTransition",
    119: "What powers Japan? Oil 37%, gas 21%, coal 17%, nuclear 5%. Post-Fukushima legacy visible in the mix.\nenergтx.com/datasets | info@energtx.com\n#Japan #Energy #Nuclear #DonutChart #DataViz #Infographic #Sustainability",
    120: "What powers the UK? Gas 36%, oil 33%. Wind growing fast. Coal nearly eliminated.\nenergтx.com/datasets | info@energtx.com\n#UK #Energy #Wind #DonutChart #DataViz #Infographic #Climate #EnergyTransition",
    121: "What powers Brazil? 50% renewable energy. Hydro dominant. The greenest major economy.\nenergтx.com/datasets | info@energtx.com\n#Brazil #Renewables #Hydropower #DonutChart #DataViz #Infographic #CleanEnergy",
    122: "What powers Turkey? Gas 29%, oil 27%, coal 27%. Hydro 7%, wind+solar growing. At the crossroads.\nenergтx.com/datasets | info@energtx.com\n#Turkey #Energy #EnergyTransition #DonutChart #DataViz #Infographic",
    123: "What powers Russia? Gas 54%, oil 21%. The gas superpower in one chart.\nenergтx.com/datasets | info@energtx.com\n#Russia #NaturalGas #Energy #DonutChart #DataViz #Infographic #Geopolitics",
    124: "What powers Australia? Coal 26%, gas 27%, oil 32%. Solar booming at 7% — highest growth rate.\nenergтx.com/datasets | info@energtx.com\n#Australia #Energy #Solar #Coal #DonutChart #DataViz #Infographic #CleanEnergy",
    125: "What powers South Korea? Oil 34%, gas 18%, coal 25%, nuclear 17%. Heavily import-dependent.\nenergтx.com/datasets | info@energtx.com\n#SouthKorea #Energy #Nuclear #DonutChart #DataViz #Infographic",
    126: "EU Big 5 electricity mix: five grids, five paths. France = nuclear. Poland = coal. Spain = solar+wind. All different.\nenergтx.com/datasets | info@energtx.com\n#EU #Europe #Electricity #SmallMultiples #DataViz #Infographic #EnergyTransition",
    127: "Post-COVID electricity: who bounced back? Some countries surged past 2019 levels. Others still haven't recovered.\nenergтx.com/datasets | info@energtx.com\n#COVID #Electricity #Recovery #DataViz #Infographic #GlobalEconomy #Energy",
    128: "Clean vs dirty electricity: top 15 generators compared. France and Brazil shine. China and India face the biggest gap.\nenergтx.com/datasets | info@energtx.com\n#CleanEnergy #Electricity #DataViz #Infographic #Climate #Sustainability #NetZero",
    129: "Where did new energy come from since 2015? Gas +6,498 TWh. Solar +4,501 TWh. Wind +4,019 TWh. Coal still grew +2,155 TWh.\nenergтx.com/datasets | info@energtx.com\n#Energy #Growth #Solar #Wind #Gas #DataViz #Infographic #Climate #EnergyTransition",
    130: "The great energy race: fossil vs renewable. Global share since 1990. Renewables gaining — but fossil fuels still dominate at 80%.\nenergтx.com/datasets | info@energtx.com\n#FossilFuels #Renewables #Climate #NetZero #DataViz #Infographic #EnergyTransition",
}

FILE_MAP = {}
for f in sorted(PNG_DIR.glob("*.png")):
    num = int(f.stem.split("_")[0])
    FILE_MAP[num] = f

def main():
    client = Client()
    client.login(HANDLE, PASSWORD)
    print(f"Logged in as {HANDLE}")
    to_post = sorted(POSTS.keys())
    total = len(to_post)
    print(f"Posting {total} infographics (30s intervals)...\n")
    for i, num in enumerate(to_post, 1):
        png_path = FILE_MAP.get(num)
        if not png_path or not png_path.exists():
            print(f"  [{i}/{total}] #{num:03d} - SKIP (file missing)")
            continue
        text = POSTS[num].replace("energтx", "energtx")
        img_data = png_path.read_bytes()
        try:
            upload = client.upload_blob(img_data)
            embed = models.AppBskyEmbedImages.Main(
                images=[models.AppBskyEmbedImages.Image(alt=text.split("\n")[0], image=upload.blob)]
            )
            client.send_post(text=text, embed=embed)
            print(f"  [{i}/{total}] #{num:03d} - Posted OK")
        except Exception as e:
            err = str(e).encode('ascii', 'replace').decode()
            print(f"  [{i}/{total}] #{num:03d} - ERROR: {err}")
        if i < total:
            time.sleep(30)
    print(f"\nDone! {total} infographics posted to @{HANDLE}")

if __name__ == "__main__":
    main()
