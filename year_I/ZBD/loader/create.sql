CREATE TABLE Achievement (
    id integer NOT NULL,
    name character varying(50) NOT NULL
);

CREATE SEQUENCE Achievement_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE Achievement_id_seq OWNED BY Achievement.id;

CREATE TABLE Arena (
    id integer NOT NULL
);

CREATE TABLE Battle (
    id character varying(100) NOT NULL,
    time timestamp with time zone NOT NULL,
    arena_id integer,
    deck_id integer NOT NULL,
    mode_id integer NOT NULL,
    opponentTeam_id integer NOT NULL,
    team_id integer NOT NULL,
    type_id integer NOT NULL
);

CREATE TABLE BattleMode (
    id integer NOT NULL,
    overtimeSeconds integer,
    sameDeck boolean,
    cardLevels_id integer,
    modeDeckType_id integer,
    playerType_id integer
);

CREATE SEQUENCE BattleMode_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE BattleMode_id_seq OWNED BY BattleMode.id;

CREATE TABLE BattleType (
    id integer NOT NULL,
    typeName character varying(50) NOT NULL
);

CREATE SEQUENCE BattleType_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE BattleType_id_seq OWNED BY BattleType.id;

CREATE TABLE Card (
    id integer NOT NULL,
    key character varying(50) NOT NULL,
    cardType_id integer
);

CREATE TABLE CardLevelsType (
    id integer NOT NULL,
    typeName character varying(50) NOT NULL
);

CREATE SEQUENCE CardLevelsType_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE CardLevelsType_id_seq OWNED BY CardLevelsType.id;

CREATE TABLE CardType (
    id integer NOT NULL,
    name character varying(50) NOT NULL
);

CREATE SEQUENCE CardType_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE CardType_id_seq OWNED BY CardType.id;

CREATE SEQUENCE Card_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE Card_id_seq OWNED BY Card.id;

CREATE TABLE Clan (
    id integer NOT NULL,
    tag character varying(100) NOT NULL,
    name character varying(50) NOT NULL
);

CREATE TABLE ClanMembership (
    id integer NOT NULL,
    donations integer NOT NULL,
    donationsReceived integer NOT NULL,
    clan_id integer NOT NULL,
    role_id integer NOT NULL
);

CREATE SEQUENCE ClanMembership_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE ClanMembership_id_seq OWNED BY ClanMembership.id;

CREATE TABLE ClanRoleType (
    id integer NOT NULL,
    roleName character varying(50) NOT NULL
);

CREATE SEQUENCE ClanRoleType_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE ClanRoleType_id_seq OWNED BY ClanRoleType.id;

CREATE SEQUENCE Clan_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE Clan_id_seq OWNED BY Clan.id;

CREATE TABLE DeckType (
    id integer NOT NULL,
    typeName character varying(50) NOT NULL
);

CREATE SEQUENCE DeckType_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE DeckType_id_seq OWNED BY DeckType.id;

CREATE TABLE MemberBattleCard (
    id integer NOT NULL,
    level integer NOT NULL,
    card_id integer NOT NULL,
    member_id integer NOT NULL
);

CREATE SEQUENCE MemberBattleCard_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE MemberBattleCard_id_seq OWNED BY MemberBattleCard.id;

CREATE TABLE ModeDeckType (
    id integer NOT NULL,
    typeName character varying(50) NOT NULL
);

CREATE SEQUENCE ModeDeckType_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE ModeDeckType_id_seq OWNED BY ModeDeckType.id;

CREATE TABLE Player (
    id character varying(100) NOT NULL,
    tag character varying(100) NOT NULL
);

CREATE TABLE PlayerAchievement (
    id integer NOT NULL,
    stars integer NOT NULL,
    value integer NOT NULL,
    target integer NOT NULL,
    achievement_id integer NOT NULL,
    playerStats_id integer NOT NULL
);

CREATE SEQUENCE PlayerAchievement_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE PlayerAchievement_id_seq OWNED BY PlayerAchievement.id;

CREATE TABLE PlayerBattleDeck (
    id integer NOT NULL,
    card_id integer NOT NULL,
    playerStats_id integer NOT NULL
);

CREATE SEQUENCE PlayerBattleDeck_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE PlayerBattleDeck_id_seq OWNED BY PlayerBattleDeck.id;

CREATE TABLE PlayerCard (
    id integer NOT NULL,
    level integer NOT NULL,
    count integer NOT NULL,
    card_id integer NOT NULL,
    playerStats_id integer NOT NULL
);

CREATE SEQUENCE PlayerCard_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE PlayerCard_id_seq OWNED BY PlayerCard.id;

CREATE TABLE PlayerGameStats (
    id integer NOT NULL,
    total integer NOT NULL,
    wins integer NOT NULL,
    draws integer NOT NULL,
    tournamentGames integer NOT NULL,
    warDayWins integer NOT NULL
);

CREATE SEQUENCE PlayerGameStats_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE PlayerGameStats_id_seq OWNED BY PlayerGameStats.id;

CREATE TABLE PlayerStats (
    id integer NOT NULL,
    name character varying(50) NOT NULL,
    trophies integer NOT NULL,
    tournamentCardsWon integer NOT NULL,
    maxTrophies integer NOT NULL,
    totalDonations integer NOT NULL,
    level integer NOT NULL,
    timestamp timestamp with time zone,
    arena_id integer NOT NULL,
    clanMembership_id integer NOT NULL,
    favouriteCard_id integer NOT NULL,
    gameStats_id integer NOT NULL,
    player_id character varying(100) NOT NULL
);

CREATE SEQUENCE PlayerStats_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE PlayerStats_id_seq OWNED BY PlayerStats.id;

CREATE TABLE PlayerTypeId (
    id integer NOT NULL,
    typeName character varying(50) NOT NULL
);

CREATE SEQUENCE PlayerTypeId_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE PlayerTypeId_id_seq OWNED BY PlayerTypeId.id;

CREATE TABLE Team (
    id integer NOT NULL,
    crownsEarned integer NOT NULL
);

CREATE TABLE TeamMember (
    id integer NOT NULL,
    name character varying(50),
    clan_id integer,
    player_id character varying(100) NOT NULL,
    team_id integer NOT NULL
);

CREATE SEQUENCE TeamMember_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE TeamMember_id_seq OWNED BY TeamMember.id;

CREATE SEQUENCE Team_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

CREATE INDEX Achievement_name_e7eb97d5_like ON Achievement USING btree (name varchar_pattern_ops);

CREATE INDEX BattleMode_cardLevels_id_69c7ccf2 ON BattleMode USING btree (cardLevels_id);

CREATE INDEX BattleMode_modeDeckType_id_9bdd49d8 ON BattleMode USING btree (modeDeckType_id);

CREATE INDEX BattleMode_playerType_id_9224212c ON BattleMode USING btree (playerType_id);

CREATE INDEX BattleType_typeName_693cc5e8_like ON BattleType USING btree (typeName varchar_pattern_ops);

CREATE INDEX Battle_arena_id_9dd71b0a ON Battle USING btree (arena_id);

CREATE INDEX Battle_deck_id_42decd97 ON Battle USING btree (deck_id);

CREATE INDEX Battle_id_8aa94897_like ON Battle USING btree (id varchar_pattern_ops);

CREATE INDEX Battle_mode_id_c899fa2e ON Battle USING btree (mode_id);

CREATE INDEX Battle_opponentTeam_id_bef686d0 ON Battle USING btree (opponentTeam_id);

CREATE INDEX Battle_team_id_da774af3 ON Battle USING btree (team_id);

CREATE INDEX Battle_type_id_64baef94 ON Battle USING btree (type_id);

CREATE INDEX CardLevelsType_typeName_ecc1408e_like ON CardLevelsType USING btree (typeName varchar_pattern_ops);

CREATE INDEX CardType_name_807f6a0d_like ON CardType USING btree (name varchar_pattern_ops);

CREATE INDEX Card_cardType_id_ae399915 ON Card USING btree (cardType_id);

CREATE INDEX ClanMembership_clan_id_01f9c375 ON ClanMembership USING btree (clan_id);

CREATE INDEX ClanMembership_role_id_7e67090b ON ClanMembership USING btree (role_id);

CREATE INDEX ClanRoleType_roleName_81bcf514_like ON ClanRoleType USING btree (roleName varchar_pattern_ops);

CREATE INDEX Clan_tag_952078ad_like ON Clan USING btree (tag varchar_pattern_ops);

CREATE INDEX DeckType_typeName_fef51744_like ON DeckType USING btree (typeName varchar_pattern_ops);

CREATE INDEX MemberBattleCard_card_id_24b1d000 ON MemberBattleCard USING btree (card_id);

CREATE INDEX MemberBattleCard_member_id_29fdefc3 ON MemberBattleCard USING btree (member_id);

CREATE INDEX ModeDeckType_typeName_5c63b2cd_like ON ModeDeckType USING btree (typeName varchar_pattern_ops);

CREATE INDEX PlayerAchievement_achievement_id_ca781248 ON PlayerAchievement USING btree (achievement_id);

CREATE INDEX PlayerAchievement_playerStats_id_2078cc30 ON PlayerAchievement USING btree (playerStats_id);

CREATE INDEX PlayerBattleDeck_card_id_5772103d ON PlayerBattleDeck USING btree (card_id);

CREATE INDEX PlayerBattleDeck_playerStats_id_7fa464f2 ON PlayerBattleDeck USING btree (playerStats_id);

CREATE INDEX PlayerCard_card_id_35bd409b ON PlayerCard USING btree (card_id);

CREATE INDEX PlayerCard_playerStats_id_2df62632 ON PlayerCard USING btree (playerStats_id);

CREATE INDEX PlayerStats_arena_id_1b57cda6 ON PlayerStats USING btree (arena_id);

CREATE INDEX PlayerStats_favouriteCard_id_54736d4f ON PlayerStats USING btree (favouriteCard_id);

CREATE INDEX PlayerStats_player_id_7dae87fe ON PlayerStats USING btree (player_id);

CREATE INDEX PlayerStats_player_id_7dae87fe_like ON PlayerStats USING btree (player_id varchar_pattern_ops);

CREATE INDEX PlayerTypeId_typeName_c6c0096d_like ON PlayerTypeId USING btree (typeName varchar_pattern_ops);

CREATE INDEX Player_id_717f19eb_like ON Player USING btree (id varchar_pattern_ops);

CREATE INDEX Player_tag_a8e44145_like ON Player USING btree (tag varchar_pattern_ops);

CREATE INDEX TeamMember_clan_id_922e9828 ON TeamMember USING btree (clan_id);

CREATE INDEX TeamMember_player_id_57b471c2 ON TeamMember USING btree (player_id);

CREATE INDEX TeamMember_player_id_57b471c2_like ON TeamMember USING btree (player_id varchar_pattern_ops);

CREATE INDEX TeamMember_team_id_6a1791b4 ON TeamMember USING btree (team_id);

ALTER SEQUENCE Team_id_seq OWNED BY Team.id;


ALTER TABLE ONLY Achievement
    ADD CONSTRAINT Achievement_name_key UNIQUE (name);

ALTER TABLE ONLY Achievement
    ADD CONSTRAINT Achievement_pkey PRIMARY KEY (id);

ALTER TABLE ONLY Arena
    ADD CONSTRAINT Arena_pkey PRIMARY KEY (id);

ALTER TABLE ONLY BattleMode
    ADD CONSTRAINT BattleMode_pkey PRIMARY KEY (id);

ALTER TABLE ONLY BattleType
    ADD CONSTRAINT BattleType_pkey PRIMARY KEY (id);

ALTER TABLE ONLY BattleType
    ADD CONSTRAINT BattleType_typeName_key UNIQUE (typeName);

ALTER TABLE ONLY Battle
    ADD CONSTRAINT Battle_pkey PRIMARY KEY (id);
    
ALTER TABLE ONLY CardLevelsType
    ADD CONSTRAINT CardLevelsType_pkey PRIMARY KEY (id);

ALTER TABLE ONLY CardLevelsType
    ADD CONSTRAINT CardLevelsType_typeName_key UNIQUE (typeName);

ALTER TABLE ONLY CardType
    ADD CONSTRAINT CardType_name_key UNIQUE (name);

ALTER TABLE ONLY CardType
    ADD CONSTRAINT CardType_pkey PRIMARY KEY (id);

ALTER TABLE ONLY Card
    ADD CONSTRAINT Card_pkey PRIMARY KEY (id);

ALTER TABLE ONLY ClanMembership
    ADD CONSTRAINT ClanMembership_pkey PRIMARY KEY (id);

ALTER TABLE ONLY ClanRoleType
    ADD CONSTRAINT ClanRoleType_pkey PRIMARY KEY (id);

ALTER TABLE ONLY ClanRoleType
    ADD CONSTRAINT ClanRoleType_roleName_key UNIQUE (roleName);

ALTER TABLE ONLY Clan
    ADD CONSTRAINT Clan_pkey PRIMARY KEY (id);

ALTER TABLE ONLY Clan
    ADD CONSTRAINT Clan_tag_key UNIQUE (tag);

ALTER TABLE ONLY DeckType
    ADD CONSTRAINT DeckType_pkey PRIMARY KEY (id);

ALTER TABLE ONLY DeckType
    ADD CONSTRAINT DeckType_typeName_key UNIQUE (typeName);

ALTER TABLE ONLY MemberBattleCard
    ADD CONSTRAINT MemberBattleCard_pkey PRIMARY KEY (id);

ALTER TABLE ONLY ModeDeckType
    ADD CONSTRAINT ModeDeckType_pkey PRIMARY KEY (id);

ALTER TABLE ONLY ModeDeckType
    ADD CONSTRAINT ModeDeckType_typeName_key UNIQUE (typeName);

ALTER TABLE ONLY PlayerAchievement
    ADD CONSTRAINT PlayerAchievement_pkey PRIMARY KEY (id);



ALTER TABLE ONLY PlayerBattleDeck
    ADD CONSTRAINT PlayerBattleDeck_pkey PRIMARY KEY (id);



ALTER TABLE ONLY PlayerCard
    ADD CONSTRAINT PlayerCard_pkey PRIMARY KEY (id);



ALTER TABLE ONLY PlayerGameStats
    ADD CONSTRAINT PlayerGameStats_pkey PRIMARY KEY (id);



ALTER TABLE ONLY PlayerStats
    ADD CONSTRAINT PlayerStats_clanMembership_id_key UNIQUE (clanMembership_id);



ALTER TABLE ONLY PlayerStats
    ADD CONSTRAINT PlayerStats_gameStats_id_key UNIQUE (gameStats_id);



ALTER TABLE ONLY PlayerStats
    ADD CONSTRAINT PlayerStats_pkey PRIMARY KEY (id);



ALTER TABLE ONLY PlayerTypeId
    ADD CONSTRAINT PlayerTypeId_pkey PRIMARY KEY (id);



ALTER TABLE ONLY PlayerTypeId
    ADD CONSTRAINT PlayerTypeId_typeName_key UNIQUE (typeName);



ALTER TABLE ONLY Player
    ADD CONSTRAINT Player_pkey PRIMARY KEY (id);



ALTER TABLE ONLY Player
    ADD CONSTRAINT Player_tag_key UNIQUE (tag);



ALTER TABLE ONLY TeamMember
    ADD CONSTRAINT TeamMember_pkey PRIMARY KEY (id);



ALTER TABLE ONLY Team
    ADD CONSTRAINT Team_pkey PRIMARY KEY (id);

ALTER TABLE ONLY BattleMode
    ADD CONSTRAINT BattleMode_cardLevels_id_69c7ccf2_fk_CardLevelsType_id FOREIGN KEY (cardLevels_id) REFERENCES CardLevelsType(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY BattleMode
    ADD CONSTRAINT BattleMode_modeDeckType_id_9bdd49d8_fk_ModeDeckType_id FOREIGN KEY (modeDeckType_id) REFERENCES ModeDeckType(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY BattleMode
    ADD CONSTRAINT BattleMode_playerType_id_9224212c_fk_PlayerTypeId_id FOREIGN KEY (playerType_id) REFERENCES PlayerTypeId(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY Battle
    ADD CONSTRAINT Battle_arena_id_9dd71b0a_fk_Arena_id FOREIGN KEY (arena_id) REFERENCES Arena(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY Battle
    ADD CONSTRAINT Battle_deck_id_42decd97_fk_DeckType_id FOREIGN KEY (deck_id) REFERENCES DeckType(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY Battle
    ADD CONSTRAINT Battle_mode_id_c899fa2e_fk_BattleMode_id FOREIGN KEY (mode_id) REFERENCES BattleMode(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY Battle
    ADD CONSTRAINT Battle_opponentTeam_id_bef686d0_fk_Team_id FOREIGN KEY (opponentTeam_id) REFERENCES Team(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY Battle
    ADD CONSTRAINT Battle_team_id_da774af3_fk_Team_id FOREIGN KEY (team_id) REFERENCES Team(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY Battle
    ADD CONSTRAINT Battle_type_id_64baef94_fk_BattleType_id FOREIGN KEY (type_id) REFERENCES BattleType(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY Card
    ADD CONSTRAINT Card_cardType_id_ae399915_fk_CardType_id FOREIGN KEY (cardType_id) REFERENCES CardType(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY ClanMembership
    ADD CONSTRAINT ClanMembership_clan_id_01f9c375_fk_Clan_id FOREIGN KEY (clan_id) REFERENCES Clan(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY ClanMembership
    ADD CONSTRAINT ClanMembership_role_id_7e67090b_fk_ClanRoleType_id FOREIGN KEY (role_id) REFERENCES ClanRoleType(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY MemberBattleCard
    ADD CONSTRAINT MemberBattleCard_card_id_24b1d000_fk_Card_id FOREIGN KEY (card_id) REFERENCES Card(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY MemberBattleCard
    ADD CONSTRAINT MemberBattleCard_member_id_29fdefc3_fk_TeamMember_id FOREIGN KEY (member_id) REFERENCES TeamMember(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY PlayerAchievement
    ADD CONSTRAINT PlayerAchievement_achievement_id_ca781248_fk_Achievement_id FOREIGN KEY (achievement_id) REFERENCES Achievement(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY PlayerAchievement
    ADD CONSTRAINT PlayerAchievement_playerStats_id_2078cc30_fk_PlayerStats_id FOREIGN KEY (playerStats_id) REFERENCES PlayerStats(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY PlayerBattleDeck
    ADD CONSTRAINT PlayerBattleDeck_card_id_5772103d_fk_Card_id FOREIGN KEY (card_id) REFERENCES Card(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY PlayerBattleDeck
    ADD CONSTRAINT PlayerBattleDeck_playerStats_id_7fa464f2_fk_PlayerStats_id FOREIGN KEY (playerStats_id) REFERENCES PlayerStats(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY PlayerCard
    ADD CONSTRAINT PlayerCard_card_id_35bd409b_fk_Card_id FOREIGN KEY (card_id) REFERENCES Card(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY PlayerCard
    ADD CONSTRAINT PlayerCard_playerStats_id_2df62632_fk_PlayerStats_id FOREIGN KEY (playerStats_id) REFERENCES PlayerStats(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY PlayerStats
    ADD CONSTRAINT PlayerStats_arena_id_1b57cda6_fk_Arena_id FOREIGN KEY (arena_id) REFERENCES Arena(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY PlayerStats
    ADD CONSTRAINT PlayerStats_clanMembership_id_b566f603_fk_ClanMembership_id FOREIGN KEY (clanMembership_id) REFERENCES ClanMembership(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY PlayerStats
    ADD CONSTRAINT PlayerStats_favouriteCard_id_54736d4f_fk_Card_id FOREIGN KEY (favouriteCard_id) REFERENCES Card(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY PlayerStats
    ADD CONSTRAINT PlayerStats_gameStats_id_c7cc87b9_fk_PlayerGameStats_id FOREIGN KEY (gameStats_id) REFERENCES PlayerGameStats(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY PlayerStats
    ADD CONSTRAINT PlayerStats_player_id_7dae87fe_fk_Player_id FOREIGN KEY (player_id) REFERENCES Player(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY TeamMember
    ADD CONSTRAINT TeamMember_clan_id_922e9828_fk_Clan_id FOREIGN KEY (clan_id) REFERENCES Clan(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY TeamMember
    ADD CONSTRAINT TeamMember_player_id_57b471c2_fk_Player_id FOREIGN KEY (player_id) REFERENCES Player(id) DEFERRABLE INITIALLY DEFERRED;



ALTER TABLE ONLY TeamMember
    ADD CONSTRAINT TeamMember_team_id_6a1791b4_fk_Team_id FOREIGN KEY (team_id) REFERENCES Team(id) DEFERRABLE INITIALLY DEFERRED;



