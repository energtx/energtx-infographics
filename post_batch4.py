#!/usr/bin/env python3
"""Post 20 energtx Batch 4 infographics to Bluesky."""

import os
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

# -- Posts --
posts = [
    {
        "file": "131_gas_dependency_top20.png",
        "text": (
            "Who depends most on natural gas?\n\n"
            "Iran leads at 68%, followed by Algeria (67%) and Egypt (53%). "
            "20 countries where gas dominates the energy mix.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #NaturalGas #ClimateData #EnergyAnalytics"
        ),
        "alt": "Lollipop chart: Top 20 countries by natural gas share of primary energy, 2024. Iran 68.4%, Algeria 66.5%, Egypt 53.3%.",
    },
    {
        "file": "132_nuclear_heatmap_35yr.png",
        "text": (
            "Nuclear power: a 35-year heatmap.\n\n"
            "France has held 70%+ nuclear share for decades. "
            "Japan\u2019s post-Fukushima collapse is stark. "
            "China rising fast from near-zero.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #Nuclear #ClimateData #EnergyAnalytics"
        ),
        "alt": "Heatmap: Nuclear share of electricity for 15 countries, 1990-2024. France consistently highest, Japan sharp decline after 2011.",
    },
    {
        "file": "133_co2_intensity_electricity.png",
        "text": (
            "Carbon intensity of electricity: two worlds.\n\n"
            "Switzerland emits just 30 gCO\u2082/kWh. "
            "Kazakhstan exceeds 800. "
            "The gap between cleanest and dirtiest grids is staggering.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #Renewables #ClimateData #EnergyAnalytics"
        ),
        "alt": "Bar chart: 10 cleanest vs 10 dirtiest electricity grids by carbon intensity (gCO2/kWh), 2024.",
    },
    {
        "file": "134_coal_phaseout_slope.png",
        "text": (
            "The coal phase-out: who\u2019s actually declining?\n\n"
            "UK dropped from 14% to 2.5%. "
            "US halved its coal share. "
            "But South Africa, India, and China remain heavily dependent.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #Coal #ClimateData #EnergyAnalytics"
        ),
        "alt": "Slope chart: Coal share of primary energy, 2010 vs 2024 for 10 major coal-using countries.",
    },
    {
        "file": "135_solar_explosion_faceted.png",
        "text": (
            "The solar explosion: 12 countries, one story.\n\n"
            "China generates 800+ TWh of solar. "
            "Every single country shows exponential growth curves. "
            "The solar era is here.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #Solar #ClimateData #EnergyAnalytics"
        ),
        "alt": "Faceted area charts: Solar electricity generation for 12 countries, 2005-present. All showing exponential growth.",
    },
    {
        "file": "136_wind_power_leaders.png",
        "text": (
            "Where does the wind blow hardest?\n\n"
            "Denmark leads with 50%+ of electricity from wind. "
            "Ireland, Uruguay, and Lithuania follow. "
            "Wind is already dominant in some grids.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #WindEnergy #ClimateData #EnergyAnalytics"
        ),
        "alt": "Dot plot: Top 15 countries by wind share of electricity, 2024. Denmark leads at over 50%.",
    },
    {
        "file": "137_g7_energy_mix.png",
        "text": (
            "G7 energy DNA: what powers the world\u2019s richest economies?\n\n"
            "France is nuclear-dominated. "
            "Canada is hydro-rich. "
            "Japan and Germany still lean on fossil fuels.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #G7 #ClimateData #EnergyAnalytics"
        ),
        "alt": "Stacked bar chart: G7 countries energy mix by source (coal, oil, gas, nuclear, hydro, solar, wind, bioenergy), 2024.",
    },
    {
        "file": "138_renewable_dumbbell.png",
        "text": (
            "Renewable energy gains: 2000 \u2192 2024.\n\n"
            "Denmark and Brazil lead in renewable share. "
            "The dumbbell chart shows how far each country has come \u2014 or hasn\u2019t.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #Renewables #ClimateData #EnergyAnalytics"
        ),
        "alt": "Dumbbell chart: Renewable share of primary energy, 2000 vs 2024, for 15 countries showing percentage point gains.",
    },
    {
        "file": "139_percapita_electricity_divide.png",
        "text": (
            "The electricity divide: richest vs poorest.\n\n"
            "Norway consumes 23+ MWh per person. "
            "Nigeria and Kenya are below 0.2 MWh. "
            "A 100x gap in access to modern energy.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #EnergyAccess #ClimateData #EnergyAnalytics"
        ),
        "alt": "Bar chart: Per capita electricity consumption, top 10 vs bottom 10 countries, showing massive global inequality.",
    },
    {
        "file": "140_hydropower_giants.png",
        "text": (
            "The hydropower giants.\n\n"
            "China generates more hydroelectricity than anyone \u2014 over 1,300 TWh. "
            "Brazil and Canada follow. "
            "Hydro remains the backbone of clean electricity.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #Hydropower #ClimateData #EnergyAnalytics"
        ),
        "alt": "Horizontal bar chart: Top 15 hydroelectric generators in TWh, 2024. China leads with 1300+ TWh.",
    },
    {
        "file": "141_renewable_heatmap_g20.png",
        "text": (
            "G20 renewable energy progress: a quarter century.\n\n"
            "This heatmap tracks the renewable share of primary energy "
            "across all G20 economies from 2000 to present. "
            "Brazil stands out. Most are still below 20%.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #Renewables #ClimateData #EnergyAnalytics"
        ),
        "alt": "Heatmap: G20 countries renewable share of primary energy, 2000-2024. Brazil consistently highest, most countries below 20%.",
    },
    {
        "file": "142_bioenergy_share.png",
        "text": (
            "Bioenergy: the overlooked renewable.\n\n"
            "Often ignored in energy debates, bioenergy is a significant "
            "share of primary energy in several countries. "
            "Who\u2019s leading?\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #Bioenergy #ClimateData #EnergyAnalytics"
        ),
        "alt": "Lollipop chart: Top 15 countries by bioenergy share of primary energy, 2024.",
    },
    {
        "file": "143_carbon_intensity_change.png",
        "text": (
            "Who cleaned up their grid?\n\n"
            "Carbon intensity change in G20 electricity since 2010. "
            "UK and Germany made huge progress. "
            "Some countries got dirtier.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #CarbonIntensity #ClimateData #EnergyAnalytics"
        ),
        "alt": "Diverging bar chart: % change in carbon intensity of electricity for G20 countries, 2010-2024.",
    },
    {
        "file": "144_energy_trilemma.png",
        "text": (
            "The energy trilemma: renewables vs per capita emissions.\n\n"
            "Higher renewable share generally means lower emissions \u2014 "
            "but wealth, population, and industrial structure matter too. "
            "Bubble size = population.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #Emissions #ClimateData #EnergyAnalytics"
        ),
        "alt": "Bubble scatter plot: Renewable share vs GHG per capita for 56 countries, colored by continent, sized by population.",
    },
    {
        "file": "145_oil_net_balance.png",
        "text": (
            "Oil: exporters vs importers.\n\n"
            "Saudi Arabia and Russia are the biggest net exporters. "
            "China, India, and Japan are massive net importers. "
            "The geopolitics of oil dependency in one chart.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #Oil #ClimateData #EnergyAnalytics"
        ),
        "alt": "Diverging bar chart: Net oil balance (production minus consumption) for top 20 countries, 2024.",
    },
    {
        "file": "146_global_electricity_stack.png",
        "text": (
            "How the world generates electricity: 1990\u2013present.\n\n"
            "Coal still dominates. Gas has surged. "
            "But solar and wind are finally visible \u2014 and growing fast. "
            "A 35-year stacked area chart.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #Electricity #ClimateData #EnergyAnalytics"
        ),
        "alt": "Stacked area chart: Global electricity generation by source (coal, gas, oil, nuclear, hydro, wind, solar, bioenergy), 1990-present.",
    },
    {
        "file": "147_clean_elec_bump.png",
        "text": (
            "Clean electricity rankings: who leads the race?\n\n"
            "A bump chart tracking the top 10 cleanest grids over 25 years. "
            "Norway has held #1 almost throughout. "
            "Watch Finland and Denmark climb.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #CleanEnergy #ClimateData #EnergyAnalytics"
        ),
        "alt": "Bump chart: Rankings of top 10 countries by low-carbon share of electricity, 2000-2024.",
    },
    {
        "file": "148_fossil_decline_g20.png",
        "text": (
            "Fossil fuel decline: who\u2019s actually reducing?\n\n"
            "G20 fossil share change since 2010. "
            "UK leads the drop. Some countries increased fossil dependency. "
            "Progress is uneven.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #FossilFuels #ClimateData #EnergyAnalytics"
        ),
        "alt": "Diverging bar chart: Change in fossil share of primary energy (pp) for G20 countries, 2010-2024.",
    },
    {
        "file": "149_lowcarbon_connected_dot.png",
        "text": (
            "Low-carbon electricity: 2000 \u2192 2024.\n\n"
            "Denmark jumped from ~20% to 80%+. "
            "UK transformed its grid. "
            "Poland barely moved. "
            "The connected dot chart tells the transition story.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #EnergyTransition #ClimateData #EnergyAnalytics"
        ),
        "alt": "Connected dot chart: Low-carbon share of electricity, 2000 vs 2024, for 10 transition economies.",
    },
    {
        "file": "150_energy_intensity.png",
        "text": (
            "Energy intensity: efficient vs wasteful economies.\n\n"
            "Nigeria and Kenya use the least energy per dollar of GDP. "
            "Russia and Canada use the most. "
            "Climate, industry, and efficiency all play a role.\n\n"
            "Download in JSON, CSV or XLSX.\n"
            "Explore \u2192 energtx.com\n"
            "Contact \u2192 info@energtx.com\n\n"
            "#EnergyData #EnergyEfficiency #ClimateData #EnergyAnalytics"
        ),
        "alt": "Bar chart: Energy per unit of GDP (kWh/$) for 10 most and 10 least energy-intensive economies, 2022.",
    },
]

# -- Login --
client = Client()
client.login(handle, password)
print(f"\u2713 Logged in as {handle}")

png_dir = Path(__file__).parent / "energtx_png"

for i, post in enumerate(posts, 1):
    img_path = png_dir / post["file"]
    if not img_path.exists():
        print(f"\u2717 Missing: {post['file']}")
        continue

    try:
        with open(img_path, "rb") as f:
            img_data = f.read()

        upload = client.upload_blob(img_data)

        images = [
            models.AppBskyEmbedImages.Image(
                alt=post["alt"],
                image=upload.blob,
            )
        ]
        embed = models.AppBskyEmbedImages.Main(images=images)

        client.send_post(text=post["text"], embed=embed)
        print(f"\u2713 Posted {i}/20: {post['file']}")

    except Exception as e:
        print(f"\u2717 Error posting {post['file']}: {e}")

    if i < 20:
        time.sleep(5)

print("\n\u2705 Done! All 20 posts published.")
