CREATE TABLE card (
	id INTEGER NOT NULL,
	name VARCHAR(20) NOT NULL,
	"maxLevel" INTEGER,
	PRIMARY KEY (id)
);

CREATE TABLE achievement (
	name VARCHAR(63) NOT NULL,
	PRIMARY KEY (name)
);


CREATE TABLE player (
	oid VARCHAR(64) NOT NULL,
	tag VARCHAR(40) NOT NULL,
	PRIMARY KEY (tag),
	UNIQUE (oid)
);


CREATE TABLE arena (
	id INTEGER NOT NULL,
	PRIMARY KEY (id)
);


CREATE TABLE clan_location (
	id INTEGER NOT NULL,
	name VARCHAR(255),
	"isCountry" BOOLEAN,
	"countryCode" VARCHAR(7),
	PRIMARY KEY (id),
	CHECK ("isCountry" IN (0, 1))
);


CREATE TABLE player_clan_data (
	"playerTag" VARCHAR(16) NOT NULL,
	"clanTag" VARCHAR(16) NOT NULL,
	timestamp DATETIME NOT NULL,
	donations INTEGER,
	"donationsReceived" INTEGER,
	"clanRank" INTEGER,
	"previousClanRank" INTEGER,
	"clanChestPoints" INTEGER,
	"clanCardsCollected" INTEGER,
	role VARCHAR(8),
	PRIMARY KEY ("playerTag", "clanTag", timestamp),
	CONSTRAINT clanrole CHECK (role IN ('member', 'elder', 'coLeader', 'leader'))
);


CREATE TABLE battle_mode (
	name VARCHAR(63) NOT NULL,
	PRIMARY KEY (name)
);


CREATE TABLE player_achievement (
	"achievementId" INTEGER NOT NULL,
	"playerTag" VARCHAR(63) NOT NULL,
	timestamp DATETIME NOT NULL,
	target INTEGER NOT NULL,
	star INTEGER NOT NULL,
	value INTEGER NOT NULL,
	PRIMARY KEY ("achievementId", "playerTag", timestamp),
	FOREIGN KEY("achievementId") REFERENCES achievement (name)
);

CREATE TABLE battle_card (
	id INTEGER NOT NULL,
	"cardId" INTEGER,
	level SMALLINT,
	PRIMARY KEY (id),
	UNIQUE ("cardId", level),
	FOREIGN KEY("cardId") REFERENCES card (id)
);

CREATE TABLE player_card (
	"playerTag" VARCHAR(40) NOT NULL,
	"cardId" INTEGER NOT NULL,
	timestamp DATETIME NOT NULL,
	level INTEGER NOT NULL,
	count INTEGER NOT NULL,
	PRIMARY KEY ("playerTag", "cardId", timestamp),
	FOREIGN KEY("playerTag") REFERENCES player (tag),
	FOREIGN KEY("cardId") REFERENCES card (id)
);

CREATE TABLE clan (
	tag VARCHAR(16) NOT NULL,
	"locationId" INTEGER,
	PRIMARY KEY (tag),
	FOREIGN KEY("locationId") REFERENCES clan_location (id)
);


CREATE TABLE battle_basic_data (
	oid VARCHAR(64) NOT NULL,
	"arenaId" INTEGER,
	"battleModeId" INTEGER,
	timestamp DATETIME,
	type VARCHAR(20),
	"deckType" VARCHAR(10),
	PRIMARY KEY (oid),
	FOREIGN KEY("arenaId") REFERENCES arena (id),
	FOREIGN KEY("battleModeId") REFERENCES battle_mode (name),
	CONSTRAINT battletype CHECK (type IN ('two_v_two', 'challenge', 'challenge_two_v_two', 'clanMate', 'clanMate_two_v_two', 'clanWarCollectionDay', 'clanWarWarDay', 'friendly', 'PvP', 'tournament')),
	CONSTRAINT decktype CHECK ("deckType" IN ('draft', 'eventDeck', 'predefined', 'slotDeck'))
);

CREATE TABLE player_statistics (
	id INTEGER NOT NULL,
	"playerTag" VARCHAR(16) NOT NULL,
	timestamp DATETIME,
	name VARCHAR(255) NOT NULL,
	"challengeCardsWon" INTEGER NOT NULL,
	"challengeMaxWins" INTEGER NOT NULL,
	"cardsFound" INTEGER NOT NULL,
	level SMALLINT NOT NULL,
	draws INTEGER NOT NULL,
	wins INTEGER NOT NULL,
	losses INTEGER NOT NULL,
	"bestTrophies" INTEGER NOT NULL,
	"threeCrownWins" INTEGER NOT NULL,
	trophies INTEGER NOT NULL,
	"tournamentCardsWon" INTEGER NOT NULL,
	"tournamentGames" INTEGER NOT NULL,
	"totalDonations" INTEGER NOT NULL,
	total INTEGER NOT NULL,
	"warDayWins" INTEGER NOT NULL,
	"favoriteCardId" INTEGER,
	PRIMARY KEY (id),
	CONSTRAINT check_cards_won_positive CHECK ("challengeCardsWon" >= 0),
	UNIQUE ("playerTag", timestamp),
	FOREIGN KEY("playerTag") REFERENCES player (tag),
	FOREIGN KEY("favoriteCardId") REFERENCES card (id)
);

CREATE TABLE deck (
	id INTEGER NOT NULL,
	card1 INTEGER NOT NULL,
	card2 INTEGER NOT NULL,
	card3 INTEGER NOT NULL,
	card4 INTEGER NOT NULL,
	card5 INTEGER NOT NULL,
	card6 INTEGER NOT NULL,
	card7 INTEGER NOT NULL,
	card8 INTEGER NOT NULL,
	PRIMARY KEY (id),
	FOREIGN KEY(card1) REFERENCES battle_card (id),
	FOREIGN KEY(card2) REFERENCES battle_card (id),
	FOREIGN KEY(card3) REFERENCES battle_card (id),
	FOREIGN KEY(card4) REFERENCES battle_card (id),
	FOREIGN KEY(card5) REFERENCES battle_card (id),
	FOREIGN KEY(card6) REFERENCES battle_card (id),
	FOREIGN KEY(card7) REFERENCES battle_card (id),
	FOREIGN KEY(card8) REFERENCES battle_card (id)
);

CREATE TABLE clan_statistics (
	"clanTag" VARCHAR(16) NOT NULL,
	timestamp DATETIME NOT NULL,
	"badgeId" INTEGER,
	type VARCHAR(63),
	name VARCHAR(255),
	"clanScore" INTEGER,
	"requiredTrophies" INTEGER,
	"donationsPerWeek" INTEGER,
	PRIMARY KEY ("clanTag", timestamp),
	FOREIGN KEY("clanTag") REFERENCES clan (tag)
);

CREATE TABLE player_battle_data (
	id INTEGER NOT NULL,
	"playerTag" VARCHAR(16) NOT NULL,
	"battleOid" VARCHAR(63) NOT NULL,
	"deckId" INTEGER NOT NULL,
	crowns INTEGER NOT NULL,
	"clanTag" VARCHAR(16),
	"trophyChange" INTEGER NOT NULL,
	"startingTrophies" INTEGER NOT NULL,
	PRIMARY KEY (id),
	UNIQUE ("playerTag", "battleOid"),
	FOREIGN KEY("playerTag") REFERENCES player (tag),
	FOREIGN KEY("battleOid") REFERENCES battle_basic_data (oid),
	FOREIGN KEY("deckId") REFERENCES deck (id),
	FOREIGN KEY("clanTag") REFERENCES clan (tag)
);


CREATE TABLE battle_pvp (
	oid VARCHAR(64) NOT NULL,
	"playerBattleDataId" INTEGER,
	"opponentBattleDataId" INTEGER,
	PRIMARY KEY (oid),
	FOREIGN KEY(oid) REFERENCES battle_basic_data (oid),
	FOREIGN KEY("playerBattleDataId") REFERENCES player_battle_data (id),
	FOREIGN KEY("opponentBattleDataId") REFERENCES player_battle_data (id)
);


CREATE TABLE battle_tvt (
	oid VARCHAR(64) NOT NULL,
	"playerBattleDataId1" INTEGER,
	"opponentBattleDataId1" INTEGER,
	"playerBattleDataId2" INTEGER,
	"opponentBattleDataId2" INTEGER,
	PRIMARY KEY (oid),
	FOREIGN KEY(oid) REFERENCES battle_basic_data (oid),
	FOREIGN KEY("playerBattleDataId1") REFERENCES player_battle_data (id),
	FOREIGN KEY("opponentBattleDataId1") REFERENCES player_battle_data (id),
	FOREIGN KEY("playerBattleDataId2") REFERENCES player_battle_data (id),
	FOREIGN KEY("opponentBattleDataId2") REFERENCES player_battle_data (id)
);