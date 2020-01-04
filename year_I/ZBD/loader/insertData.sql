
COPY Player (id, tag) FROM stdin;
5b72ecb39e22c00001595419	P82CYV82L
9VVC89RJP	9VVC89RJP
9C2P9RLR8	9C2P9RLR8
\.

COPY Arena (id) FROM stdin;
1
\.

COPY ClanRoleType (id, roleName) FROM stdin;
1	member
\.

COPY Clan (id, tag, name) FROM stdin;
1	98C0QVQG	وارزنان
2	2UGGV8L8	АЛАШ
3	P080909V	شباب العراق
\.


COPY ClanMembership (id, donations, donationsReceived, clan_id, role_id) FROM stdin;
1	0	0	1	1
\.

COPY CardType (id, name) FROM stdin;
1	Troop
2	Building
3	Spell
\.

COPY Card (id, key, cardType_id) FROM stdin;
26000014	musketeer	1
26000013	bomber	1
27000001	goblin-hut	2
28000001	arrows	3
28000000	fireball	3
26000000	knight	1
26000001	archers	1
26000011	valkyrie	1
26000019	spear-goblins	1
26000005	minions	1
26000003	giant	1
26000016	prince	1
26000021	hog-rider	\N
26000015	baby-dragon	\N
26000018	mini-pekka	\N
26000007	witch	\N
26000044	hunter	\N
28000004	goblin-barrel	\N
\.


COPY PlayerGameStats (id, total, wins, draws, tournamentGames, warDayWins) FROM stdin;
1	17	1	15	0	0
\.

COPY PlayerStats (id, name, trophies, tournamentCardsWon, maxTrophies, totalDonations, level, timestamp, arena_id, clanMembership_id, favouriteCard_id, gameStats_id, player_id) FROM stdin;
1	خالد	30	0	30	2	3	\N	1	1	26000014	1	5b72ecb39e22c00001595419
\.

COPY PlayerTypeId (id, typeName) FROM stdin;
1	PvP
\.

COPY Team (id, crownsEarned) FROM stdin;
1	1
2	3
\.

COPY Achievement (id, name) FROM stdin;
1	Team Player
2	Friend in Need
3	Road to Glory
4	Gatherer
5	TV Royale
6	Tournament Rewards
7	Tournament Host
8	Tournament Player
9	Challenge Streak
10	Practice with Friends
11	Special Challenge
12	Friend in Need II
\.

COPY ModeDeckType (id, typeName) FROM stdin;
1	Collection
\.

COPY BattleType (id, typeName) FROM stdin;
1	PvP
\.

COPY CardLevelsType (id, typeName) FROM stdin;
1	Ladder
\.

COPY DeckType (id, typeName) FROM stdin;
1	slotDeck
\.

COPY BattleMode (id, overtimeSeconds, sameDeck, cardLevels_id, modeDeckType_id, playerType_id) FROM stdin;
1	60	f	1	1	1
\.

COPY Battle (id, time, arena_id, deck_id, mode_id, opponentTeam_id, team_id, type_id) FROM stdin;
5b72ebfa9e22c0000159106c	2018-08-10 10:06:13+00	1	1	1	2	1	1
\.

COPY TeamMember (id, name, clan_id, player_id, team_id) FROM stdin;
1	NURSULTAN	2	9VVC89RJP	1
2	مجتبى معذبهم	3	9C2P9RLR8	2
\.

COPY MemberBattleCard (id, level, card_id, member_id) FROM stdin;
1	2	26000021	1
2	2	26000015	1
3	3	26000018	1
4	4	26000005	1
5	1	28000000	1
6	4	28000001	1
7	3	26000014	1
8	1	26000007	1
9	3	26000021	2
10	1	26000015	2
11	1	26000044	2
12	3	26000018	2
13	2	28000004	2
14	2	28000000	2
15	3	26000014	2
16	1	26000016	2
\.

COPY PlayerAchievement (id, stars, value, target, achievement_id, playerStats_id) FROM stdin;
1	3	4	1	1	1
2	0	2	25	2	1
3	0	1	2	3	1
4	0	12	20	4	1
5	3	1	1	5	1
6	0	0	1000	6	1
7	0	0	1	7	1
8	0	0	1	8	1
9	0	0	4	9	1
10	1	1	5	10	1
11	0	0	1	11	1
12	0	2	5000	12	1
\.

COPY PlayerBattleDeck (id, card_id, playerStats_id) FROM stdin;
1	26000000	1
2	26000001	1
3	26000011	1
4	26000019	1
5	26000005	1
6	26000003	1
7	26000014	1
8	26000016	1
\.

COPY PlayerCard (id, level, count, card_id, playerStats_id) FROM stdin;
1	3	5	26000013	1
2	2	0	27000001	1
3	3	8	28000001	1
4	1	1	28000000	1
5	4	1	26000000	1
6	4	6	26000001	1
7	2	2	26000011	1
8	1	2	26000019	1
9	2	0	26000005	1
10	2	0	26000003	1
11	2	0	26000014	1
12	1	1	26000016	1
\.
