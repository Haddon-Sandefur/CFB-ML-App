from datetime import date
from bs4 import BeautifulSoup
import urllib3
import pandas as pd

# Get webpage
url = "https://contests.covers.com/consensus/topconsensus/ncaaf"
http = urllib3.PoolManager()
response = http.request('GET', url)
soup = BeautifulSoup(response.data)

# Pull data:
data = pd.read_html(soup.prettify())

# Format:
df = pd.concat(data, ignore_index=True)

df['Matchup'] = df['Matchup'].str.split().str[1:]
df['Consensus'] = df['Consensus'].str.split()

copy1 = df[['Matchup', 'Consensus']]
copy2 = df[['Matchup', 'Consensus']]

# Create a new column with lists of pairs
copy1['Team_Consensus_Pairs'] = copy1.apply(lambda row: list(zip(row['Matchup'], row['Consensus'])), axis=1)
copy2['Team_Consensus_Pairs'] = copy2.apply(lambda row: list(zip(row['Matchup'], row['Consensus'])), axis=1)

# Create new columns for each team and its corresponding consensus
copy1[['Team1', 'Team2']] = pd.DataFrame(copy1['Team_Consensus_Pairs'].apply(lambda x: [i[0] for i in x]).tolist())
copy1[['Consensus1', 'Consensus2']] = pd.DataFrame(copy1['Team_Consensus_Pairs'].apply(lambda x: [i[1] for i in x]).tolist())

copy2[['Team2', 'Team1']] = pd.DataFrame(copy2['Team_Consensus_Pairs'].apply(lambda x: [i[0] for i in x]).tolist())
copy2[['Consensus2', 'Consensus1']] = pd.DataFrame(copy2['Team_Consensus_Pairs'].apply(lambda x: [i[1] for i in x]).tolist())

copy1['gameId'] = copy1.index
copy2['gameId'] = copy2.index

# Drop the original 'Team_Consensus_Pairs' column
copy1 = copy1.drop('Team_Consensus_Pairs', axis=1)
copy2 = copy2.drop('Team_Consensus_Pairs', axis=1)

# Display the resulting DataFrame
final = pd.concat([copy1, copy2], ignore_index=True)
final = final.sort_values(by = 'gameId')
final.drop(['Matchup', 'Consensus'], axis=1, inplace=True)
final['datePull'] = date.today()
final['rowId'] = final.index

# Save
final.to_csv(f'../downstream/consensusSpread_{date.today()}.csv', index=False)