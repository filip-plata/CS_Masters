-- How often the game ends in a draw? More of a trivia query.
-- To add more processing - grouping by arenas.

SELECT draws.arena_id,
       SUM(draws.is_draw) / COUNT(draws) AS frequency FROM
(SELECT b.arena_id,
  CASE
  WHEN t.crowns_earned = o.crowns_earned
    THEN 1 ELSE 0
  END AS is_draw
FROM battle b
INNER JOIN Team t ON t.id = b.team_id
INNER JOIN Team o ON o.id = b.opponent_id) draws
GROUP BY draws.arena_id;