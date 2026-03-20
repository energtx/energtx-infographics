"""
energtx — Bluesky Infographic Poster
Posts all infographics with captions to @energtx.bsky.social
"""

import os
import time
import sys
from pathlib import Path
from atproto import Client, models

# ── Config ──
ENV_FILE = Path(__file__).parent / ".env.bsky"
PNG_DIR = Path(__file__).parent / "png"

# ── Load .env ──
env = {}
for line in ENV_FILE.read_text().strip().splitlines():
    if "=" in line and not line.startswith("#"):
        k, v = line.split("=", 1)
        env[k.strip()] = v.strip()

HANDLE = env.get("BLUESKY_HANDLE", "")
PASSWORD = env.get("BLUESKY_APP_PASSWORD", "")

if not PASSWORD:
    print("ERROR: BLUESKY_APP_PASSWORD is empty in .env.bsky")
    print("Please add your App Password and run again.")
    sys.exit(1)

# ── Post texts (number → text) ──
POSTS = {
    2: "The cleanest electricity grids in the world — under 50 gCO₂/kWh. Hydro, nuclear, and wind make it possible.\nenergтx.com/datasets · info@energtx.com\n#CarbonIntensity #CleanElectricity #DataViz #RStats #ggplot2 #TidyTuesday",
    3: "The dirtiest electricity grids — over 500 gCO₂/kWh. Coal is the culprit. These countries need the biggest transformation.\nenergтx.com/datasets · info@energtx.com\n#CarbonIntensity #Coal #Emissions #DataViz #RStats #ggplot2 #TidyTuesday",
    4: "Hydropower: the original renewable. Brazil, Canada, China lead — but it's geography that decides.\nenergтx.com/datasets · info@energtx.com\n#Hydropower #Renewables #CleanEnergy #DataViz #RStats #ggplot2 #TidyTuesday",
    5: "Which countries generate the most electricity from low-carbon sources? France, Sweden, Norway — nearly 100%.\nenergтx.com/datasets · info@energtx.com\n#LowCarbon #CleanEnergy #Electricity #DataViz #RStats #ggplot2 #TidyTuesday",
    6: "Some countries still get 95%+ of their energy from fossil fuels. The dependency is staggering.\nenergтx.com/datasets · info@energtx.com\n#FossilFuels #EnergyDependency #EnergyTransition #DataViz #RStats #TidyTuesday",
    7: "Solar electricity generation: exponential growth across 10 countries. China alone generates more solar than the next 5 combined.\nenergтx.com/datasets · info@energtx.com\n#Solar #SolarEnergy #Renewables #DataViz #RStats #ggplot2 #TidyTuesday",
    8: "Wind power generation since 2000 — from near zero to hundreds of TWh. China, the US, and Germany lead.\nenergтx.com/datasets · info@energtx.com\n#WindEnergy #WindPower #Renewables #DataViz #RStats #ggplot2 #TidyTuesday",
    9: "G7 energy mix side by side. France = nuclear. Canada = hydro. US = gas. Same club, wildly different strategies.\nenergтx.com/datasets · info@energtx.com\n#G7 #EnergyMix #EnergyPolicy #DataViz #RStats #ggplot2 #TidyTuesday",
    10: "Richer countries use more electricity. But the relationship isn't linear — some nations are far more efficient per dollar.\nenergтx.com/datasets · info@energtx.com\n#Electricity #GDP #EnergyEfficiency #ScatterPlot #DataViz #RStats #TidyTuesday",
    11: "China generates 9,500+ TWh. India is at 2,000 TWh and climbing fast. Two billion people, two very different trajectories.\nenergтx.com/datasets · info@energtx.com\n#China #India #Electricity #EnergyDemand #DataViz #RStats #TidyTuesday",
    12: "How America powers itself: coal is dying, gas is king, and solar+wind are the fastest growing.\nenergтx.com/datasets · info@energtx.com\n#US #EnergyMix #NaturalGas #Renewables #DataViz #RStats #ggplot2 #TidyTuesday",
    13: "France gets ~70% of electricity from nuclear. No other major economy comes close. The cleanest major grid in Europe.\nenergтx.com/datasets · info@energtx.com\n#France #Nuclear #CleanEnergy #Electricity #DataViz #RStats #TidyTuesday",
    14: "Four Nordic countries, four energy strategies. Norway = hydro. Sweden = hydro+nuclear. Denmark = wind. Finland = nuclear+bio.\nenergтx.com/datasets · info@energtx.com\n#Nordic #EnergyMix #Renewables #CleanEnergy #DataViz #RStats #TidyTuesday",
    15: "Denmark crossed 50% wind electricity. No other country is even close. The world's wind champion.\nenergтx.com/datasets · info@energtx.com\n#Denmark #WindEnergy #Renewables #EnergyTransition #DataViz #RStats #TidyTuesday",
    16: "Australia's solar generation went from near zero to leading the developed world. Rooftop solar + desert sun.\nenergтx.com/datasets · info@energtx.com\n#Australia #Solar #SolarEnergy #Renewables #DataViz #RStats #TidyTuesday",
    17: "Poland still gets ~70% of electricity from coal. Wind and solar are growing — but coal's grip is hard to break.\nenergтx.com/datasets · info@energtx.com\n#Poland #Coal #EnergyTransition #EU #DataViz #RStats #ggplot2 #TidyTuesday",
    18: "Norway: 98% clean electricity, but 60%+ fossil primary energy. Oil exports fund the cleanest grid. The ultimate paradox.\nenergтx.com/datasets · info@energtx.com\n#Norway #Hydro #Oil #EnergyParadox #DataViz #RStats #ggplot2 #TidyTuesday",
    19: "The UK went from almost zero wind power to being the world's offshore wind leader in 20 years.\nenergтx.com/datasets · info@energtx.com\n#UK #OffshoreWind #WindEnergy #Renewables #DataViz #RStats #TidyTuesday",
    20: "Brazil: 80%+ renewable electricity, mostly hydro. Now adding wind and solar. South America's clean energy powerhouse.\nenergтx.com/datasets · info@energtx.com\n#Brazil #Renewables #Hydropower #CleanEnergy #DataViz #RStats #TidyTuesday",
    21: "Japan before Fukushima: 30% nuclear. After: near zero. Now nuclear is slowly coming back. A decade of energy whiplash.\nenergтx.com/datasets · info@energtx.com\n#Japan #Nuclear #Fukushima #EnergyTransition #DataViz #RStats #TidyTuesday",
    22: "Saudi Arabia: 99% fossil energy. But solar is emerging. The oil kingdom is building one of the world's largest solar farms.\nenergтx.com/datasets · info@energtx.com\n#SaudiArabia #Solar #Oil #Vision2030 #DataViz #RStats #ggplot2 #TidyTuesday",
    23: "South Korea imports 95% of its energy. Coal + nuclear + LNG keep the lights on. Solar is growing — but the gap is enormous.\nenergтx.com/datasets · info@energtx.com\n#SouthKorea #Energy #EnergyImports #Nuclear #DataViz #RStats #TidyTuesday",
    24: "BRICS energy DNA: Brazil = hydro. Russia = gas. India = coal. China = coal + solar. South Africa = coal.\nenergтx.com/datasets · info@energtx.com\n#BRICS #EnergyMix #GlobalEnergy #DataViz #RStats #ggplot2 #TidyTuesday",
    25: "Who cut fossil fuel use since 2000? Who didn't? A dumbbell chart tells the full story.\nenergтx.com/datasets · info@energtx.com\n#FossilFuels #Emissions #ClimateChange #DumbbellChart #DataViz #RStats #TidyTuesday",
    26: "A heatmap of renewable electricity share across 15 countries, 2000–present. Watch the energy transition unfold tile by tile.\nenergтx.com/datasets · info@energtx.com\n#Renewables #Heatmap #EnergyTransition #DataViz #RStats #ggplot2 #TidyTuesday",
    27: "Global electricity generation as a treemap. Size = TWh. Color = how clean. China dominates in volume.\nenergтx.com/datasets · info@energtx.com\n#Electricity #Treemap #GlobalEnergy #DataViz #RStats #ggplot2 #TidyTuesday",
    28: "Bioenergy: the overlooked renewable. Some countries get 30%+ of primary energy from biomass.\nenergтx.com/datasets · info@energtx.com\n#Bioenergy #Biomass #Renewables #CleanEnergy #DataViz #RStats #TidyTuesday",
    29: "Electricity demand since 2010: some countries up 100%+, others flat or declining. The growth map is shifting.\nenergтx.com/datasets · info@energtx.com\n#Electricity #EnergyDemand #Growth #DataViz #RStats #ggplot2 #TidyTuesday",
    30: "Nuclear electricity share by country. France at 70%+. Most countries under 20%. A small club with outsized impact.\nenergтx.com/datasets · info@energtx.com\n#Nuclear #NuclearEnergy #CleanEnergy #DataViz #RStats #ggplot2 #TidyTuesday",
    31: "Chile: from zero renewables to 30%+ solar+wind in a decade. The Atacama Desert is now a giant solar farm.\nenergтx.com/datasets · info@energtx.com\n#Chile #Solar #WindEnergy #LatinAmerica #DataViz #RStats #TidyTuesday",
    32: "South Africa gets 85%+ of electricity from coal. Decarbonization is a social and economic challenge, not just technical.\nenergтx.com/datasets · info@energtx.com\n#SouthAfrica #Coal #EnergyTransition #Africa #DataViz #RStats #TidyTuesday",
    33: "Indonesia: coal went from 30% to 60%+ of electricity in 20 years. The opposite of a transition.\nenergтx.com/datasets · info@energtx.com\n#Indonesia #Coal #SoutheastAsia #Energy #DataViz #RStats #ggplot2 #TidyTuesday",
    34: "Turkey's electricity: gas dependence peaking, coal holding steady, and hydro+wind+solar climbing.\nenergтx.com/datasets · info@energtx.com\n#Turkey #Energy #EnergyTransition #Renewables #DataViz #RStats #TidyTuesday",
    35: "Egypt's electricity generation tripled since 1990. A young, growing population driving demand higher.\nenergтx.com/datasets · info@energtx.com\n#Egypt #Electricity #Africa #EnergyDemand #DataViz #RStats #TidyTuesday",
    36: "UAE: from 100% fossil to nuclear + solar. Barakah nuclear plant and massive solar farms are changing the Gulf story.\nenergтx.com/datasets · info@energtx.com\n#UAE #Nuclear #Solar #EnergyDiversification #DataViz #RStats #TidyTuesday",
    37: "Spain's solar generation is surging. Sun-rich geography + policy support = one of Europe's fastest solar curves.\nenergтx.com/datasets · info@energtx.com\n#Spain #Solar #SolarEnergy #Europe #DataViz #RStats #ggplot2 #TidyTuesday",
    38: "Italy: Europe's most gas-dependent major economy. 40%+ of electricity from natural gas.\nenergтx.com/datasets · info@energtx.com\n#Italy #NaturalGas #Energy #Europe #DataViz #RStats #ggplot2 #TidyTuesday",
    39: "G20 fossil fuel footprint ranked. 80% of global emissions from this group alone.\nenergтx.com/datasets · info@energtx.com\n#G20 #FossilFuels #Emissions #CarbonFootprint #DataViz #RStats #ggplot2 #TidyTuesday",
    40: "EU average renewable energy share: from ~8% in 2000 to 20%+ today. Steady climb, but 80% still fossil or nuclear.\nenergтx.com/datasets · info@energtx.com\n#EU #Renewables #EnergyTransition #Europe #DataViz #RStats #TidyTuesday",
    41: "Energy inequality is extreme. The top-consuming country uses 50x more energy per person than the bottom.\nenergтx.com/datasets · info@energtx.com\n#EnergyInequality #EnergyPoverty #PerCapita #DataViz #RStats #TidyTuesday",
    42: "Solar vs wind: which bet are countries making? Geography and policy decide the balance.\nenergтx.com/datasets · info@energtx.com\n#Solar #Wind #Renewables #ScatterPlot #DataViz #RStats #ggplot2 #TidyTuesday",
    43: "Global fossil fuel share: 86% in 1990. Still ~80% today. Three decades of climate summits and the needle barely moved.\nenergтx.com/datasets · info@energtx.com\n#FossilFuels #Climate #EnergyTransition #DataViz #RStats #ggplot2 #TidyTuesday",
    44: "Global solar + wind generation: from 50 TWh in 2000 to 4,000+ TWh today. The growth curve is exponential.\nenergтx.com/datasets · info@energtx.com\n#Solar #Wind #Renewables #CleanEnergy #DataViz #RStats #ggplot2 #TidyTuesday",
    45: "Renewable vs fossil energy by continent. South America leads in renewables. Asia still overwhelmingly fossil.\nenergтx.com/datasets · info@energtx.com\n#Continents #Renewables #FossilFuels #GlobalEnergy #DataViz #RStats #TidyTuesday",
    46: "Canada: 60%+ hydro, 15% nuclear. One of the cleanest major grids. Wind and solar are the new additions.\nenergтx.com/datasets · info@energtx.com\n#Canada #Hydropower #Nuclear #CleanEnergy #DataViz #RStats #TidyTuesday",
    47: "Sweden: nearly 100% clean electricity from hydro + nuclear + wind. One of the most decarbonized grids on the planet.\nenergтx.com/datasets · info@energtx.com\n#Sweden #CleanEnergy #Hydro #Nuclear #Wind #DataViz #RStats #TidyTuesday",
    48: "Netherlands: the Groningen gas field powered Europe for decades. Now it's closing. Dutch gas dependency is finally declining.\nenergтx.com/datasets · info@energtx.com\n#Netherlands #NaturalGas #Groningen #Energy #DataViz #RStats #TidyTuesday",
    49: "Mexico: 85%+ fossil energy. Massive solar and wind potential — barely tapped.\nenergтx.com/datasets · info@energtx.com\n#Mexico #Energy #FossilFuels #Renewables #DataViz #RStats #TidyTuesday",
    50: "Argentina sits on Vaca Muerta — the world's second-largest shale gas reserve. Gas is now 55%+ of primary energy.\nenergтx.com/datasets · info@energtx.com\n#Argentina #NaturalGas #VacaMuerta #ShaleGas #DataViz #RStats #TidyTuesday",
    51: "Nuclear power: Chernobyl and Fukushima changed everything. Two decades of stagnation. Now a global renaissance.\nenergтx.com/datasets · info@energtx.com\n#Nuclear #NuclearEnergy #Chernobyl #Fukushima #DataViz #RStats #TidyTuesday",
    52: "Global coal consumption is STILL growing. Despite every climate pledge, every COP, every net-zero target.\nenergтx.com/datasets · info@energtx.com\n#Coal #FossilFuels #Climate #Emissions #DataViz #RStats #ggplot2 #TidyTuesday",
    53: "Fastest renewable energy growth since 2015: who added the most percentage points?\nenergтx.com/datasets · info@energtx.com\n#Renewables #EnergyTransition #Growth #CleanEnergy #DataViz #RStats #TidyTuesday",
    54: "60 years of global energy in a stacked area chart. Coal, oil, gas, nuclear, hydro, wind, solar — all flowing together.\nenergтx.com/datasets · info@energtx.com\n#Energy #StackedArea #DataViz #RStats #ggplot2 #TidyTuesday",
    55: "More renewables = less fossil dependency? The scatter plot says yes — but it's not a straight line.\nenergтx.com/datasets · info@energtx.com\n#Renewables #FossilFuels #ScatterPlot #DataViz #RStats #TidyTuesday",
    56: "Coal dependency: the two extremes. Some countries at 50%+, others below 1%. The gap is enormous.\nenergтx.com/datasets · info@energtx.com\n#Coal #EnergyDependency #DataViz #RStats #ggplot2 #TidyTuesday",
    57: "Solar electricity by continent: range and average. The spread within each continent is staggering.\nenergтx.com/datasets · info@energtx.com\n#Solar #Continents #Renewables #DataViz #RStats #ggplot2 #TidyTuesday",
    58: "Europe's Big Four renewables race: Germany vs UK vs France vs Spain. Who's winning?\nenergтx.com/datasets · info@energtx.com\n#EU #Renewables #EnergyTransition #DataViz #RStats #ggplot2 #TidyTuesday",
    59: "The electricity titans: US vs China vs India. Three curves, three completely different trajectories.\nenergтx.com/datasets · info@energtx.com\n#US #China #India #Electricity #DataViz #RStats #TidyTuesday",
    60: "Nuclear power race: France (stable), US (flat), China (surging), South Korea (steady). Four strategies.\nenergтx.com/datasets · info@energtx.com\n#Nuclear #NuclearEnergy #France #China #DataViz #RStats #TidyTuesday",
    61: "Gas giants: who depends most on natural gas? Russia, US, Iran, Saudi Arabia — four very different stories.\nenergтx.com/datasets · info@energtx.com\n#NaturalGas #Energy #Russia #US #DataViz #RStats #TidyTuesday",
    62: "Wind share race: Denmark leads at 50%+. UK, Germany, Spain trailing. Europe's wind leaders compared.\nenergтx.com/datasets · info@energtx.com\n#WindEnergy #Denmark #Europe #Renewables #DataViz #RStats #TidyTuesday",
    63: "Solar share race: Australia leads among developed nations. China, US, India, Japan compared.\nenergтx.com/datasets · info@energtx.com\n#Solar #Australia #China #Renewables #DataViz #RStats #TidyTuesday",
    64: "G20: Fossil vs Renewable energy side by side. Brazil leads renewables. Saudi Arabia leads fossil dependency.\nenergтx.com/datasets · info@energtx.com\n#G20 #FossilFuels #Renewables #DataViz #RStats #ggplot2 #TidyTuesday",
    65: "Electricity per capita: three worlds. Gulf States 15,000+ kWh. Nordic 10,000+. South Asia under 1,000.\nenergтx.com/datasets · info@energtx.com\n#Electricity #PerCapita #EnergyInequality #DataViz #RStats #TidyTuesday",
    66: "Clean baseload: hydro vs nuclear. Countries with 30%+ combined. France, Sweden, Norway, Brazil, Canada lead.\nenergтx.com/datasets · info@energtx.com\n#Hydro #Nuclear #CleanEnergy #Baseload #DataViz #RStats #TidyTuesday",
    67: "Oil: producers vs consumers. Some countries produce far more than they consume. Others are pure importers.\nenergтx.com/datasets · info@energtx.com\n#Oil #Production #Consumption #EnergyTrade #DataViz #RStats #TidyTuesday",
    68: "Energy DNA: EU vs US vs China. Three superpowers, three completely different energy mixes.\nenergтx.com/datasets · info@energtx.com\n#EU #US #China #EnergyMix #DataViz #RStats #ggplot2 #TidyTuesday",
    69: "Decade of change: renewable energy growth 2014 vs 2024. Who added the most percentage points?\nenergтx.com/datasets · info@energtx.com\n#Renewables #EnergyTransition #DumbbellChart #DataViz #RStats #TidyTuesday",
    70: "Coal phase-out at four speeds: UK (fast), Germany (steady), Poland (slow), Turkey (barely).\nenergтx.com/datasets · info@energtx.com\n#Coal #PhaseOut #EnergyTransition #DataViz #RStats #TidyTuesday",
    71: "Middle East energy: four petrostates compared. Oil + gas dominate. UAE adding nuclear + solar.\nenergтx.com/datasets · info@energtx.com\n#MiddleEast #Oil #Gas #EnergyMix #DataViz #RStats #TidyTuesday",
    72: "Southeast Asia: four grids compared. Indonesia = coal. Vietnam = hydro+coal. Thailand = gas.\nenergтx.com/datasets · info@energtx.com\n#SoutheastAsia #Energy #Coal #Hydro #DataViz #RStats #TidyTuesday",
    73: "Latin America: five electricity strategies. Brazil = hydro. Chile = solar+wind. Mexico = gas.\nenergтx.com/datasets · info@energtx.com\n#LatinAmerica #Energy #Hydropower #Solar #DataViz #RStats #TidyTuesday",
    74: "G20 electricity per capita over time. A heatmap showing who consumes most — and how it's changing.\nenergтx.com/datasets · info@energtx.com\n#G20 #Electricity #PerCapita #Heatmap #DataViz #RStats #TidyTuesday",
    75: "Fossil fuel dependency in a heatmap. 15 countries, 25 years. Watch some countries break free — and others stay locked.\nenergтx.com/datasets · info@energtx.com\n#FossilFuels #Heatmap #EnergyTransition #DataViz #RStats #TidyTuesday",
    76: "Natural gas production: the big three. US dominates. Russia second. Iran third. The gas geopolitics are shifting.\nenergтx.com/datasets · info@energtx.com\n#NaturalGas #Production #US #Russia #DataViz #RStats #TidyTuesday",
    77: "Oil production: the three superpowers. US, Saudi Arabia, Russia. A decade of leadership changes.\nenergтx.com/datasets · info@energtx.com\n#Oil #Production #OPEC #US #DataViz #RStats #TidyTuesday",
    78: "Coal consumption: the divergence. China surging. India rising. US collapsing. Three lines, three eras.\nenergтx.com/datasets · info@energtx.com\n#Coal #China #India #US #DataViz #RStats #TidyTuesday",
    79: "Energy per capita: the 50x gap. Top 10 vs Bottom 10 countries. Same planet, different centuries.\nenergтx.com/datasets · info@energtx.com\n#EnergyInequality #PerCapita #GlobalEnergy #DataViz #RStats #TidyTuesday",
    80: "The 90% Club: countries with near-zero carbon grids. France, Sweden, Norway, Brazil, Canada. Different recipes, same result.\nenergтx.com/datasets · info@energtx.com\n#CleanEnergy #LowCarbon #Electricity #DataViz #RStats #TidyTuesday",
    81: "Electricity trade: who imports, who exports? Some countries are net exporters. Others depend entirely on imports.\nenergтx.com/datasets · info@energtx.com\n#Electricity #Trade #Imports #Exports #DataViz #RStats #TidyTuesday",
    82: "Global renewable electricity treemap. Size = TWh. Color = renewable share. China generates most, but share is low.\nenergтx.com/datasets · info@energtx.com\n#Renewables #Treemap #GlobalEnergy #DataViz #RStats #ggplot2 #TidyTuesday",
    83: "Energy appetite: who grew, who shrank since 2000? The growth map is shifting dramatically East and South.\nenergтx.com/datasets · info@energtx.com\n#Energy #Growth #Consumption #DataViz #RStats #ggplot2 #TidyTuesday",
    84: "Grid decarbonization: watch it happen. Carbon intensity of electricity across 15 countries, 2000–present.\nenergтx.com/datasets · info@energtx.com\n#CarbonIntensity #Decarbonization #Heatmap #DataViz #RStats #TidyTuesday",
    85: "China vs everyone else: solar dominance. China now generates more solar electricity than the rest of the world combined.\nenergтx.com/datasets · info@energtx.com\n#China #Solar #Renewables #DataViz #RStats #ggplot2 #TidyTuesday",
    86: "China vs everyone else: wind power. China's wind generation now rivals all other countries combined.\nenergтx.com/datasets · info@energtx.com\n#China #Wind #WindEnergy #Renewables #DataViz #RStats #TidyTuesday",
    87: "Biggest greenhouse gas emitters ranked. The top 5 account for over 60% of global emissions.\nenergтx.com/datasets · info@energtx.com\n#GHG #Emissions #Climate #DataViz #RStats #ggplot2 #TidyTuesday",
    88: "Who demands the most electricity per person? Gulf states and Nordic countries at the top.\nenergтx.com/datasets · info@energtx.com\n#Electricity #PerCapita #EnergyDemand #DataViz #RStats #TidyTuesday",
    89: "Germany vs Japan: two energy transitions. Germany bet on renewables. Japan lost nuclear and is rebuilding.\nenergтx.com/datasets · info@energtx.com\n#Germany #Japan #EnergyTransition #Renewables #DataViz #RStats #TidyTuesday",
    90: "UK: how to kill coal in 15 years. From 40% to near zero. Gas + wind replaced it. A success story in data.\nenergтx.com/datasets · info@energtx.com\n#UK #Coal #WindEnergy #EnergyTransition #DataViz #RStats #TidyTuesday",
    91: "India's energy mix: coal dominance under pressure. Solar and wind growing, but coal is still 50%+ of energy.\nenergтx.com/datasets · info@energtx.com\n#India #Coal #Solar #EnergyMix #DataViz #RStats #TidyTuesday",
    92: "China's energy transformation: coal declining as a share while solar, wind, and nuclear surge.\nenergтx.com/datasets · info@energtx.com\n#China #EnergyTransition #Coal #Solar #DataViz #RStats #TidyTuesday",
    93: "Russia: the gas superpower. Natural gas dominates 50%+ of primary energy. Oil second. Renewables barely visible.\nenergтx.com/datasets · info@energtx.com\n#Russia #NaturalGas #Energy #FossilFuels #DataViz #RStats #TidyTuesday",
    94: "Electrification trend: how much energy is electricity? Rising everywhere. China leads the shift.\nenergтx.com/datasets · info@energtx.com\n#Electrification #Electricity #Energy #DataViz #RStats #TidyTuesday",
    95: "Alpine clean energy: Austria vs Switzerland. Both hydro-powered, but different nuclear strategies.\nenergтx.com/datasets · info@energtx.com\n#Austria #Switzerland #Hydro #CleanEnergy #DataViz #RStats #TidyTuesday",
    96: "Small countries, big ambitions. Portugal and Ireland — both pushing toward 50%+ renewable electricity.\nenergтx.com/datasets · info@energtx.com\n#Portugal #Ireland #Renewables #SmallCountries #DataViz #RStats #TidyTuesday",
    97: "How the world makes electricity: 40 years of global generation by source. Coal still king, but the mix is shifting.\nenergтx.com/datasets · info@energtx.com\n#Electricity #GlobalEnergy #EnergyMix #DataViz #RStats #ggplot2 #TidyTuesday",
    98: "Coal electricity: China + India vs the world. Together they burn more coal for power than everyone else combined.\nenergтx.com/datasets · info@energtx.com\n#Coal #China #India #Electricity #DataViz #RStats #TidyTuesday",
    99: "Nuclear power by country: 25 years in a heatmap. France consistently at 70%+. Others rising and falling.\nenergтx.com/datasets · info@energtx.com\n#Nuclear #Heatmap #Electricity #DataViz #RStats #ggplot2 #TidyTuesday",
    100: "Global electricity by continent. Asia generates 16,741 TWh — more than Europe and Americas combined.\nenergтx.com/datasets · info@energtx.com\n#Electricity #Continents #Treemap #GlobalEnergy #DataViz #RStats #TidyTuesday",
}

# ── File mapping ──
FILE_MAP = {}
png_files = sorted(PNG_DIR.glob("*.png"))
for f in png_files:
    num = int(f.stem.split("_")[0])
    FILE_MAP[num] = f

# ── Skip broken chart ──
SKIP = {1}  # 01_energy_intensity is empty

def main():
    client = Client()
    client.login(HANDLE, PASSWORD)
    print(f"Logged in as {HANDLE}")

    to_post = sorted(set(POSTS.keys()) - SKIP)
    total = len(to_post)
    print(f"Posting {total} infographics...\n")

    for i, num in enumerate(to_post, 1):
        png_path = FILE_MAP.get(num)
        if not png_path or not png_path.exists():
            print(f"  [{i}/{total}] #{num:03d} — SKIP (file missing)")
            continue

        text = POSTS[num]
        # Fix energтx typo to energtx
        text = text.replace("energтx", "energtx")

        # Read image
        img_data = png_path.read_bytes()

        try:
            # Upload image
            upload = client.upload_blob(img_data)

            # Create post with image
            embed = models.AppBskyEmbedImages.Main(
                images=[
                    models.AppBskyEmbedImages.Image(
                        alt=text.split("\n")[0],
                        image=upload.blob,
                    )
                ]
            )

            client.send_post(text=text, embed=embed)
            print(f"  [{i}/{total}] #{num:03d} — Posted ✓")

        except Exception as e:
            print(f"  [{i}/{total}] #{num:03d} — ERROR: {e}")

        # Rate limit: wait between posts
        if i < total:
            time.sleep(5)

    print(f"\nDone! {total} infographics posted to @{HANDLE}")

if __name__ == "__main__":
    main()
