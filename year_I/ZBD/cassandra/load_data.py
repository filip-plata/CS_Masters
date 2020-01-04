import os
import json
import uuid
import cassandra
import cassandra.query as cq

from datetime import datetime
from cassandra.cluster import Cluster


BASE_DIR = os.path.dirname(os.path.abspath(__file__))
BATTLE_DATA_FILE = os.path.join(BASE_DIR,
    "clashroyale_stable_battles_20180924.jsonl")
BATCH_LIMIT = 16
KEY_SPACE = 'fp371335'

cluster = Cluster(['lkdb'])
session = cluster.connect(KEY_SPACE)


insert_battle = session.prepare(
    "INSERT INTO clashroyale_battles (id, player_tag, utc_time, clan_tag," \
    "arena_id, team_size, win, draw, loss, type, deck_type, deck)" \
    "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
batch = cq.BatchStatement()


with open(BATTLE_DATA_FILE, "r+") as handle:
    rows = 0
    for i, line in enumerate(handle):
        data = json.loads(line)
        players = data["opponent"] + data["team"]
        max_crowns = max(data["opponentCrowns"], data["teamCrowns"])
        min_crowns = min(data["opponentCrowns"], data["teamCrowns"])

        for player in players:
            clan_tag = player["clan"]["tag"] if ("clan" in player) else None
            deck = list(map(lambda d: tuple(d.values()), player["deck"]))
            arena_id = data["arena"]["arenaID"] if "arenaID" in data["arena"] else None

            if max_crowns == min_crowns:
                win = 0
                draw = 1
                loss = 0
            else:
                win = 1 if player["crownsEarned"] == max_crowns else 0
                draw = 0
                loss = 1 if player["crownsEarned"] == min_crowns else 0

            rows += 1
            batch.add(insert_battle,
                (uuid.uuid4(), player["tag"],
                datetime.utcfromtimestamp(data["utcTime"]), clan_tag,
                arena_id, data["teamSize"], win, draw, loss,
                data["type"], data["deckType"], deck))
        if i % BATCH_LIMIT == 0:
            print("Batch nr %d" % i)
            session.execute(batch)
            batch.clear()

session.execute(batch)
print("Rows %d" % rows)

cluster.shutdown()
