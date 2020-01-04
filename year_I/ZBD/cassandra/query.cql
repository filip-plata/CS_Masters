SELECT player_tag, SUM(CAST(draw AS bigint)) AS draws, SUM(CAST(win AS bigint)) AS wins, SUM(CAST(loss AS bigint)) AS loss
FROM fp371335.clashroyale_battles
WHERE utc_time >= '2018-03-01' AND utc_time <= '2018-04-01'
GROUP BY player_tag
ALLOW FILTERING;

SELECT player_tag, SUM(CAST(draw AS bigint)) AS draws, SUM(CAST(win AS bigint)) AS wins, SUM(CAST(loss AS bigint)) AS loss FROM fp371335.clashroyale_battles WHERE utc_time >= '2018-03-01' AND utc_time <= '2018-04-01' GROUP BY player_tag ALLOW FILTERING;


SELECT SUM(CAST(draw AS bigint)) AS draws, SUM(CAST(win AS bigint)) AS wins, SUM(CAST(loss AS bigint)) AS loss
FROM fp371335.clashroyale_battles
WHERE utc_time >= '2018-03-01' AND utc_time <= '2018-04-01'
ALLOW FILTERING;

