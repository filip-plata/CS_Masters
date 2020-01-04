-- Query returns cards which are most often on the winning side
-- in battles

SELECT card.name, top_cards.level
FROM card
INNER JOIN
(SELECT mbc.card_id, mbc.level,
       1.0 * SUM(CASE WHEN
         t.crowns_earned > o.crowns_earned
         THEN 1 ELSE 0 END) / COUNT(*)
         AS frequency
FROM Team t
INNER JOIN Battle b ON b.id = t.id
INNER JOIN Team o ON o.id = b.opponent_id
INNER JOIN team_member tm on tm.team_id = t.id
INNER JOIN member_battle_card mbc on tm.id = mbc.team_member_id
GROUP BY mbc.card_id, mbc.level
ORDER BY frequency DESC
LIMIT 10) top_cards
ON top_cards.card_id = card.id;