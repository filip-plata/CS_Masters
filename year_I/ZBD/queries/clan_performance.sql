-- Clans that do the best - have the most wins

SELECT clan.name, clan.tag, clan_results.frequency
FROM clan
INNER JOIN

(SELECT clan.tag,
       1.0 * SUM(CASE WHEN
         t.crowns_earned > o.crowns_earned
         THEN 1 ELSE 0 END) / COUNT(*) AS frequency
FROM team t
INNER JOIN team_member tmb ON tmb.team_id = t.id
INNER JOIN clan ON clan.tag = tmb.clan_tag
INNER JOIN battle b ON b.team_id = t.id
INNER JOIN team o ON b.opponent_id = o.id
GROUP BY clan.tag
ORDER BY frequency DESC
LIMIT 20) clan_results

ON clan.tag = clan_results.tag;