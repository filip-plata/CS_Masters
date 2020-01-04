EXPLAIN ANALYZE WITH
  player_team_battles AS (
        SELECT
                sum(CASE WHEN t1.crownsEarned > t2.crownsEarned THEN 1 ELSE 0 END) AS wins,
                sum(CASE WHEN t1.crownsEarned = t2.crownsEarned THEN 1 ELSE 0 END) AS ties,
                sum(CASE WHEN t1.crownsEarned < t2.crownsEarned THEN 1 ELSE 0 END) AS losses,
		TeamMember.player_id AS player_id
        FROM Battle b 
          INNER JOIN Team t1 ON b.team_id = t1.id
          INNER JOIN Team t2 ON b.opponentTeam_id = t2.id
          INNER JOIN TeamMember ON b.team_id = TeamMember.team_id
        WHERE
          b.time >= :'TIME_START' AND b.time <= :'TIME_END'
        GROUP BY
          TeamMember.player_id
        ),

  player_opponent_battles AS (
        SELECT
                sum(CASE WHEN t1.crownsEarned < t2.crownsEarned THEN 1 ELSE 0 END) AS wins,
                sum(CASE WHEN t1.crownsEarned = t2.crownsEarned THEN 1 ELSE 0 END) AS ties,
                sum(CASE WHEN t1.crownsEarned > t2.crownsEarned THEN 1 ELSE 0 END) AS losses,
		TeamMember.player_id AS player_id
        FROM Battle b 
          INNER JOIN Team t1 ON b.team_id = t1.id
          INNER JOIN Team t2 ON b.opponentTeam_id = t2.id
          INNER JOIN TeamMember ON b.opponentTeam_id = TeamMember.team_id
        WHERE
          b.time >= :'TIME_START' AND b.time <= :'TIME_END'
        GROUP BY
          TeamMember.player_id
        )

SELECT 
	COALESCE(sum(sub.wins)+sum(sub.losses)+sum(sub.ties),0) as total, 
	COALESCE(sum(sub.wins),0) as wins, 
	COALESCE(sum(sub.losses),0) as losses,
	COALESCE(sum(sub.ties),0) as ties,
	player_id
FROM ((SELECT * FROM player_team_battles) UNION ALL (SELECT * FROM player_opponent_battles)) AS sub GROUP BY player_id;
