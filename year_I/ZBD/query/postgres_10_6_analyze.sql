                                                                             QUERY PLAN                                                                              
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
 Sort  (cost=423.70..423.71 rows=4 width=33) (actual time=0.105..0.106 rows=9 loops=1)
   Sort Key: battle."time"
   Sort Method: quicksort  Memory: 25kB
   ->  Nested Loop  (cost=6.95..423.66 rows=4 width=33) (actual time=0.040..0.097 rows=9 loops=1)
         ->  Nested Loop  (cost=6.07..403.87 rows=4 width=22) (actual time=0.029..0.050 rows=9 loops=1)
               ->  Nested Loop  (cost=5.64..401.98 rows=4 width=22) (actual time=0.024..0.029 rows=9 loops=1)
                     ->  Index Scan using player_tag_key on player  (cost=0.43..8.45 rows=1 width=16) (actual time=0.012..0.012 rows=1 loops=1)
                           Index Cond: ((tag)::text = 'P2822YCPP'::text)
                     ->  Bitmap Heap Scan on teammember  (cost=5.21..392.54 rows=100 width=19) (actual time=0.010..0.012 rows=9 loops=1)
                           Recheck Cond: ((player_id)::text = (player.id)::text)
                           Heap Blocks: exact=2
                           ->  Bitmap Index Scan on teammember_player_id_57b471c2_like  (cost=0.00..5.18 rows=100 width=0) (actual time=0.006..0.006 rows=9 loops=1)
                                 Index Cond: ((player_id)::text = (player.id)::text)
               ->  Index Only Scan using team_pkey on team  (cost=0.43..0.47 rows=1 width=4) (actual time=0.002..0.002 rows=1 loops=9)
                     Index Cond: (id = teammember.team_id)
                     Heap Fetches: 9
         ->  Bitmap Heap Scan on battle  (cost=0.89..4.93 rows=2 width=23) (actual time=0.004..0.004 rows=1 loops=9)
               Recheck Cond: ((team_id = team.id) OR (opponentteam_id = team.id))
               Filter: (("time" >= '2018-08-10 00:00:00+02'::timestamp with time zone) AND ("time" < '2018-09-30 00:00:00+02'::timestamp with time zone))
               Heap Blocks: exact=9
               ->  BitmapOr  (cost=0.89..0.89 rows=2 width=0) (actual time=0.003..0.003 rows=0 loops=9)
                     ->  Bitmap Index Scan on battle_team_id_da774af3  (cost=0.00..0.44 rows=1 width=0) (actual time=0.002..0.002 rows=1 loops=9)
                           Index Cond: (team_id = team.id)
                     ->  Bitmap Index Scan on battle_opponentteam_id_bef686d0  (cost=0.00..0.44 rows=1 width=0) (actual time=0.001..0.001 rows=0 loops=9)
                           Index Cond: (opponentteam_id = team.id)
 Planning time: 4.230 ms
 Execution time: 0.157 ms
(27 rows)

