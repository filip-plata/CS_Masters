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
    clanMembership_id integer,
    threeCrownWins integer NOT NULL,
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
    name character varying(50) NOT NULL,
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