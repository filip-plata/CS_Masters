INSERT INTO arena (id) VALUES (1);
INSERT INTO battle_mode (name) VALUES ('Ladder');
INSERT INTO battle_basic_data (oid, "arenaId", "battleModeId", timestamp, type, "deckType") VALUES
 ('5b72ebfa9e22c0000159106c', 1, 'Ladder', '2018-08-10 12:06:13.000000', 'PvP', 'slotDeck');
INSERT INTO battle_card (id, "cardId", level) VALUES
((1, 26000021, 2), (2, 26000015, 2), (3, 26000018, 3), (4, 26000005, 4), (5, 28000000, 1), (6, 28000001, 4), (7, 26000014, 3), (8, 26000007, 1));
INSERT INTO deck (id, card1, card2, card3, card4, card5, card6, card7, card8) VALUES (1, 1, 2, 3, 4, 5, 6, 7, 8);
INSERT INTO player_battle_data (id, "playerTag", "battleOid", "deckId", crowns, "clanTag", "trophyChange", "startingTrophies") VALUES
 (1, '9VVC89RJP', '5b72ebfa9e22c0000159106c', 1, 1, '2UGGV8L8', 0, 0);
INSERT INTO battle_card (id, "cardId", level) VALUES
((9, 26000021, 3), (10, 26000015, 1), (11, 26000044, 1), (12, 28000004, 2), (13, 28000000, 2), (14, 26000016, 1));
INSERT INTO deck (id, card1, card2, card3, card4, card5, card6, card7, card8) VALUES
(2, 9, 10, 11, 3, 12, 13, 7, 14);
INSERT INTO player_battle_data (id, "playerTag", "battleOid", "deckId", crowns, "clanTag", "trophyChange", "startingTrophies") VALUES
(2, '9C2P9RLR8', '5b72ebfa9e22c0000159106c', 2, 3, 'P080909V', 0, 0);
INSERT INTO battle_pvp (oid, "playerBattleDataId", "opponentBattleDataId") VALUES
('5b72ebfa9e22c0000159106c', 1, 2);
