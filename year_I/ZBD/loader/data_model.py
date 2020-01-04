import json
import argparse
import datetime
import functools as ft
import sys

import sqlalchemy as sa
from sqlalchemy.orm import sessionmaker
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()

NonNullColumn = ft.partial(sa.Column, nullable=False)


class Achievement(Base):
    __tablename__ = 'achievement'

    id = NonNullColumn(sa.Integer, primary_key=True)
    name = NonNullColumn(sa.String(50))


class CardType(Base):
    __tablename__ = 'cardtype'

    id = NonNullColumn(sa.Integer, primary_key=True)
    name = NonNullColumn(sa.String(50), unique=True)


class ClanRoleType(Base):
    __tablename__ = 'clanroletype'

    id = sa.Column(sa.Integer, primary_key=True)
    rolename = NonNullColumn(sa.String(50), unique=True)


class Card(Base):
    __tablename__ = 'card'

    id = NonNullColumn(sa.Integer, primary_key=True)
    key = NonNullColumn(sa.String(50), unique=True)
    cardtype_id = sa.Column(sa.Integer, sa.ForeignKey(CardType.id))


class Player(Base):
    __tablename__ = 'player'

    id = NonNullColumn(sa.Integer, primary_key=True)
    tag = sa.Column(sa.String(100), unique=True)


class Arena(Base):
    __tablename__ = 'arena'

    id = NonNullColumn(sa.Integer, primary_key=True)


class Clan(Base):
    __tablename__ = 'clan'

    id = NonNullColumn(sa.Integer, primary_key=True)
    tag = sa.Column(sa.String(50), unique=True)
    name = NonNullColumn(sa.String(50))


class ModeDeckType(Base):
    __tablename__ = 'modedecktype'

    id = NonNullColumn(sa.Integer, primary_key=True)
    typename = sa.Column(sa.String(50), unique=True)


class CardLevelsType(Base):
    __tablename__ = 'cardlevelstype'

    id = NonNullColumn(sa.Integer, primary_key=True)
    typename = sa.Column(sa.String(50), unique=True)


class PlayerTypeId(Base):
    __tablename__ = 'playertypeid'

    id = NonNullColumn(sa.Integer, primary_key=True)
    typename = NonNullColumn(sa.String(50), unique=True)


class BattleMode(Base):
    __tablename__ = 'battlemode'
    id = NonNullColumn(sa.Integer, primary_key=True)
    overtimeseconds = sa.Column(sa.Integer)
    playertype_id = sa.Column(sa.Integer, sa.ForeignKey(PlayerTypeId.id))
    modedecktype_id = sa.Column(sa.Integer, sa.ForeignKey(ModeDeckType.id))
    samedeck = sa.Column(sa.Boolean)
    cardlevels_id = sa.Column(sa.Integer, sa.ForeignKey(CardLevelsType.id))


class DeckType(Base):
    __tablename__ = 'decktype'

    id = NonNullColumn(sa.Integer, primary_key=True)
    typename = sa.Column(sa.String(50), unique=True)


class BattleType(Base):
    __tablename__ = 'battletype'

    id = NonNullColumn(sa.Integer, primary_key=True)
    typename = NonNullColumn(sa.String(50), unique=True)


class Team(Base):
    __tablename__ = 'team'

    id = NonNullColumn(sa.Integer, primary_key=True)
    crownsearned = sa.Column(sa.Integer)


class Battle(Base):
    __tablename__ = 'battle'

    id = NonNullColumn(sa.String(100), primary_key=True)

    team_id = NonNullColumn(sa.Integer, sa.ForeignKey(Team.id))
    opponentteam_id = NonNullColumn(sa.Integer, sa.ForeignKey(Team.id))
    time = NonNullColumn(sa.DateTime)
    arena_id = NonNullColumn(sa.Integer, sa.ForeignKey(Arena.id))

    deck_id = NonNullColumn(sa.Integer, sa.ForeignKey(DeckType.id))
    type_id = NonNullColumn(sa.Integer, sa.ForeignKey(BattleType.id))
    mode_id = sa.Column(sa.Integer, sa.ForeignKey(BattleMode.id))


class TeamMember(Base):
    __tablename__ = 'teammember'

    id = NonNullColumn(sa.Integer, primary_key=True)

    player_id = NonNullColumn(sa.Integer, sa.ForeignKey(Player.id))
    clan_id = sa.Column(sa.Integer, sa.ForeignKey(Clan.id))
    team_id = NonNullColumn(sa.Integer,
                            sa.ForeignKey(Team.id))
    name = NonNullColumn(sa.String(50))

    __table_args__ = (
        sa.UniqueConstraint(team_id, player_id),
        {},
    )


class MemberBattleCard(Base):
    __tablename__ = 'memberbattlecard'

    id = NonNullColumn(sa.Integer, primary_key=True)

    member_id = sa.Column(sa.Integer, sa.ForeignKey(TeamMember.id))
    card_id = sa.Column(sa.Integer, sa.ForeignKey(Card.id))
    level = sa.Column(sa.SmallInteger)


class ClanMembership(Base):
    __tablename__ = 'clanmembership'

    id = NonNullColumn(sa.Integer, primary_key=True)
    clan_id = sa.Column(sa.Integer, sa.ForeignKey(Clan.id))
    role_id = sa.Column(sa.Integer, sa.ForeignKey(ClanRoleType.id))

    donations = sa.Column(sa.Integer)
    donationsreceived = sa.Column(sa.Integer)


class PlayerGameStats(Base):
    __tablename__ = 'playergamestats'

    id = sa.Column(sa.Integer, primary_key=True)

    total = NonNullColumn(sa.Integer)
    wins = NonNullColumn(sa.Integer)
    draws = NonNullColumn(sa.Integer)
    losses = NonNullColumn(sa.Integer)

    tournamentgames = NonNullColumn(sa.Integer)
    wardaywins = NonNullColumn(sa.Integer)


class PlayerStats(Base):
    __tablename__ = 'playerstats'

    id = NonNullColumn(sa.Integer, primary_key=True)

    player_id = NonNullColumn(sa.Integer, sa.ForeignKey(Player.id))
    name = NonNullColumn(sa.String(50))
    trophies = NonNullColumn(sa.Integer)
    arena_id = NonNullColumn(sa.Integer, sa.ForeignKey(Arena.id))
    tournamentcardswon = NonNullColumn(sa.Integer)
    maxtrophies = NonNullColumn(sa.Integer)
    totaldonations = NonNullColumn(sa.Integer)
    level = NonNullColumn(sa.SmallInteger)
    favouritecard_id = sa.Column(sa.Integer, sa.ForeignKey(Card.id))
    threecrownwins = NonNullColumn(sa.Integer)
    timestamp = sa.Column(sa.DateTime)
    clanmembership_id = sa.Column(
        sa.Integer, sa.ForeignKey(ClanMembership.id))
    gamestats_id = NonNullColumn(sa.Integer, sa.ForeignKey(PlayerGameStats.id))


class PlayerAchievement(Base):
    __tablename__ = 'playerachievement'

    id = NonNullColumn(sa.Integer, primary_key=True)

    playerstats_id = sa.Column(
        sa.Integer, sa.ForeignKey(PlayerStats.id))
    achievement_id = sa.Column(sa.Integer,
                               sa.ForeignKey(Achievement.id))

    stars = NonNullColumn(sa.Integer)
    value = NonNullColumn(sa.Integer)
    target = NonNullColumn(sa.Integer)

    __table_args__ = (
        sa.UniqueConstraint(playerstats_id, achievement_id),
        {},
    )


class PlayerBattleDeck(Base):
    __tablename__ = 'playerbattledeck'

    id = NonNullColumn(sa.Integer, primary_key=True)

    playerstats_id = sa.Column(
        sa.Integer, sa.ForeignKey(PlayerStats.id))

    card_id = sa.Column(sa.Integer, sa.ForeignKey(Card.id))


class PlayerCard(Base):
    __tablename__ = 'playercard'

    id = NonNullColumn(sa.Integer, primary_key=True)

    card_id = NonNullColumn(sa.ForeignKey(Card.id))

    playerstats_id = NonNullColumn(
        sa.Integer, sa.ForeignKey(PlayerStats.id))

    level = NonNullColumn(sa.Integer)
    count = NonNullColumn(sa.Integer)


class Loader:
    UNCOMMITTED_LIMIT = 400000

    def __init__(self, session_db):
        self.obj_to_save = []
        self.session = session_db
        self.arenas = set(map(lambda a: a.id, session_db.query(Arena).all()))
        self.players = dict(map(lambda p: (p.tag, p.id), session_db.query(Player).all()))
        self.cardTypes = dict(map(lambda a: (a.name, a.id), session_db.query(CardType).all()))
        self.cards = set(map(lambda a: a.id, session_db.query(Card).all()))
        self.achievements = {}
        self.clans = dict(map(lambda p: (p.tag, p.id), session_db.query(Clan).all()))
        self.clan_roles = {}
        self.battle_cards = {}

        self.player_types = {}
        self.mode_deck_types = {}
        self.card_levels = {}
        self.battle_types = {}
        self.deck_types = {}
        self.battle_modes = {}

        self.battles = 0
        self.player_battle_deck_count = 0
        self.player_team_member_count = 0
        self.player_statistics_count = 0
        self.player_cards = 0
        self.player_achievements = 0
        self.teams_count = 0
        self.member_battle_card_count = 0

    def parse_battle_data(self, data):
        utc = datetime.datetime.fromtimestamp(data["utcTime"])

        if "arena" in data and data["arena"]:
            self.process_arena_data(data["arena"])
        self.process_battle_mode_data(data["mode"])

        team_id = self.process_team_data(data["team"], data["teamCrowns"])
        opponent_id = self.process_team_data(data["opponent"], data["opponentCrowns"])

        self.process_basic_battle_data(data, utc, team_id, opponent_id)

    def process_team_data(self, team_data, crowns):
        self.teams_count += 1
        self.add_object(Team(id=self.teams_count,
                             crownsearned=crowns))
        for player_info in team_data:
            pi = player_info
            self.process_player_battle_data(pi)
        return self.teams_count

    def process_player_battle_data(self, player_battle_info):
        self.player_team_member_count += 1
        pbi = player_battle_info
        clan_tag = pbi["clan"]["tag"] if "clan" in pbi else None
        player_tag = pbi["tag"]

        if clan_tag and clan_tag not in self.clans:
            clan_id = len(self.clans) + 1
            self.add_object(Clan(id=clan_id, tag=clan_tag, name=pbi["clan"]["name"]))
            self.clans[clan_tag] = clan_id

        if player_tag not in self.players:
            id_ = len(self.players) + 1

            self.add_object(Player(id=id_, tag=player_tag))
            self.players[player_tag] = id_

        self.add_object(TeamMember(
            id=self.player_team_member_count,
            team_id=self.teams_count,
            player_id=self.players.get(pbi["tag"]),
            clan_id=self.clans.get(clan_tag),
            name=pbi["name"]))

        for card in pbi['deck']:
            card_id = card["id"]
            if card_id not in self.cards:
                self.add_object(Card(id=card_id, key=card["key"]))
                self.cards.add(card_id)

            self.add_object(MemberBattleCard(
                id=self.member_battle_card_count,
                member_id=self.player_team_member_count,
                card_id=card_id,
                level=card['level']))

            self.member_battle_card_count += 1

    def process_basic_battle_data(self, data, timestamp, t_id, o_id):
        self._create_dictionary_object(self.battle_types, BattleType,
                                       data["type"], "typename")
        self._create_dictionary_object(self.deck_types, DeckType,
                                       data["deckType"], "typename")

        self.battles += 1
        self.add_object(Battle(
            id=self.battles,
            time=timestamp,
            team_id=t_id,
            opponentteam_id=o_id,
            type_id=self.battle_types[data["type"]],
            arena_id=data.get("arena").get("arenaID"),
            mode_id=self.battle_modes[data["mode"]["name"]],
            deck_id=self.deck_types[data["deckType"]]))

    def process_battle_mode_data(self, mode_data):
        if "players" in mode_data:
            self._create_dictionary_object(self.player_types, PlayerTypeId,
                                           mode_data["players"], "typename")

        if "deck" in mode_data:
            self._create_dictionary_object(
                self.mode_deck_types, ModeDeckType,
                mode_data["deck"], "typename")

        if "cardLevels" in mode_data:
            self._create_dictionary_object(
                self.card_levels, CardLevelsType,
                mode_data["cardLevels"], "typename")

        if mode_data["name"] not in self.battle_modes:
            self.battle_modes[mode_data["name"]] = len(self.battle_modes) + 1
            self.add_object(BattleMode(
                id=self.battle_modes[mode_data["name"]],
                playertype_id=self.player_types.get(mode_data.get("players")),
                cardlevels_id=self.card_levels.get(mode_data.get("cardLevels")),
                modedecktype_id=self.mode_deck_types.get(mode_data.get("deck")),
                samedeck=mode_data.get("sameDeck"),
                overtimeseconds=mode_data.get("overtimeSeconds"),
            ))

    def parse_player_profile_data(self, data):
        utc = datetime.datetime.fromtimestamp(data["_utcTime"])
        player_tag = data["tag"]

        arena_id = self.process_arena_data(data["arena"])
        clan_data = data.get("clan")
        membership_id = None
        if clan_data:
            membership_id = self.process_clan_data(clan_data)

        self.process_basic_player_data(player_tag)
        self.process_player_statistics(
            data["stats"], data["games"],
            player_tag, data["name"],
            data["trophies"], arena_id,
            utc, membership_id)
        self.process_cards_data(data["cards"])
        self.process_achievements_data(data["achievements"])

        for deckCardId in data['deckLink'].split('=')[1].split(';'):
            self.add_object(PlayerBattleDeck(
                id=self.player_battle_deck_count,
                playerstats_id=self.player_statistics_count,
                card_id=deckCardId))

            self.player_battle_deck_count += 1


    def process_player_statistics(self, stats, games, player_tag,
                                  player_name, trophies, arena_id,
                                  timestamp, membership_id):
        self.player_statistics_count += 1
        self.add_object(PlayerGameStats(
            id=self.player_statistics_count,
            draws=games["draws"],
            wins=games["wins"],
            total=games["total"],
            tournamentgames=games["tournamentGames"],
            wardaywins=games["warDayWins"]))

        self.add_object(PlayerStats(
            id=self.player_statistics_count,
            timestamp=timestamp,
            player_id=self.players[player_tag],
            arena_id=arena_id,
            favouritecard_id=stats["favoriteCard"]["id"],
            maxtrophies=stats["maxTrophies"],
            totaldonations=stats["totalDonations"],
            tournamentcardswon=stats["tournamentCardsWon"],
            name=player_name,
            level=stats["level"],
            trophies=trophies,
            threecrownwins=stats["threeCrownWins"],
            gamestats_id=self.player_statistics_count,
            clanmembership_id=membership_id))

    def process_clan_data(self, clan):
        clan_tag = clan["tag"]

        if clan_tag not in self.clans:
            clan_id = len(self.clans) + 1
            self.add_object(Clan(id=clan_id, tag=clan_tag, name=clan["name"]))
            self.clans[clan_tag] = clan_id

        if clan["role"] not in self.clan_roles:
            role_id = len(self.clan_roles) + 1
            self.add_object(ClanRoleType(id=role_id,
                                         rolename=clan["role"]))
            self.clan_roles[clan["role"]] = role_id

        membership_id = self.player_statistics_count+1

        self.add_object(ClanMembership(
            id=membership_id,
            clan_id=self.clans[clan_tag],
            role_id=self.clan_roles[clan["role"]],
            donations=clan["donations"],
            donationsreceived=clan["donationsReceived"]))

        return membership_id

    def process_arena_data(self, arena):
        arena_id = arena["arenaID"]

        if arena_id not in self.arenas:
            self.add_object(Arena(id=arena_id))
            self.arenas.add(arena_id)

        return arena_id

    def process_basic_player_data(self, player_tag):
        if player_tag not in self.players:
            id_ = len(self.players) + 1

            self.add_object(Player(id=id_, tag=player_tag))
            self.players[player_tag] = id_

    def process_cards_data(self, cards):
        for card in cards:
            card_id = card["id"]

            if card_id not in self.cards:
                card_type_id = None
                if 'type' in card:
                    type = card['type']
                    if type in self.cardTypes:
                        card_type_id = self.cardTypes[type]
                    else:
                        card_type_id = len(self.cardTypes) + 1

                        self.add_object(CardType(id=card_type_id, name=type))
                        self.cardTypes[type] = card_type_id

                self.add_object(Card(id=card_id, key=card["key"], cardtype_id=card_type_id))
                self.cards.add(card_id)

            self.player_cards += 1
            player_card_id = self.player_cards
            self.add_object(PlayerCard(id=player_card_id,
                playerstats_id=self.player_statistics_count, level=card["level"], count=card["count"],
                card_id=card_id))

    def process_achievements_data(self, achievements):
        for achievement in achievements:
            achievement_name = achievement["name"]

            if achievement_name not in self.achievements:
                id_ = len(self.achievements) + 1
                self.add_object(Achievement(id=id_, name=achievement_name))
                self.achievements[achievement_name] = id_

            self.player_achievements += 1
            id_ = self.player_achievements
            self.add_object(PlayerAchievement(id=id_,
                achievement_id=self.achievements[achievement_name],
                playerstats_id=self.player_statistics_count,
                target=achievement["target"],
                value=achievement["value"],
                stars=achievement["stars"]))

    def add_object(self, obj):
        self.obj_to_save.append(obj)

        if self.uncommitted_objects >= self.UNCOMMITTED_LIMIT:
            self.flush_objects()

    def _create_dictionary_object(self, dict_, constructor, data, field, **kwargs):
        if data not in dict_:
            id_ = len(dict_) + 1
            kwargs[field] = data
            kwargs["id"] = id_
            self.add_object(constructor(**kwargs))
            dict_[data] = id_

    @property
    def uncommitted_objects(self):
        return len(self.obj_to_save)

    def flush_objects(self):
        self.session.bulk_save_objects(self.obj_to_save)
        self.obj_to_save.clear()
        self.session.commit()

    def __enter__(self):
        return self

    def __exit__(self, type_, value, tb):
        self.flush_objects()


def arg_parser():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "profiles_filepath",
        help="Path to a file with profiles data in jsonl format")
    parser.add_argument(
        "battles_filepath",
        help="Path to a file with battles data in jsonl format")
    parser.add_argument("--ddl_filepath",
                        help="Create schema before inserting data")
    parser.add_argument("connection_string",
                        help="database://user:password@host:port/database")

    parser.add_argument("--create-tables", dest='create_tables', action='store_true',
                        help="Create tables using sqlalchemy")

    return parser.parse_args()


def create_schema(connection_string, ddl_filepath):
    if connection_string.startswith("postgresql://"):
        import psycopg2
        with open(ddl_filepath, "r") as ddl:
            with psycopg2.connect(connection_string) as connection:
                with connection.cursor() as cursor:
                    cursor.execute(ddl.read())
    else:
        raise ValueError("Only postgres is supported")


def load_data(session, profiles, battles):
    count = 0

    with Loader(session) as loader:
        with open(profiles, "r") as f:
            for line in f:
                count += 1
                loader.parse_player_profile_data(json.loads(line))

                if count % 100 == 0:
                    print(count)

        with open(battles, "r") as f:
            for line in f:
                count += 1
                loader.parse_battle_data(json.loads(line))

                if count % 100 == 0:
                    print(count)


if __name__ == "__main__":
    if len(sys.argv) == 1:
        for table in Base.__subclasses__():
            name = table.__tablename__
            print("CREATE VIEW %s AS SELECT * FROM fp371335.%s;" % (name, name))
    else:
        args = arg_parser()

        engine = sa.create_engine(args.connection_string, echo=False)

        Session = sessionmaker(bind=engine)
        sess = Session()

        if args.ddl_filepath:
            create_schema(args.connection_string, args.ddl_filepath)
        if args.create_tables:
            Base.metadata.create_all(engine)
        load_data(sess, args.profiles_filepath, args.battles_filepath)
