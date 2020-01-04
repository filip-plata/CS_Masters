                                                                              QUERY PLAN                                                                              
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
 Sort  (cost=296672.93..296673.31 rows=154 width=187) (actual time=121849.662..121849.683 rows=9 loops=1)
   Sort Key: battle."time"
   Sort Method:  quicksort  Memory: 25kB
   ->  Hash Join  (cost=250853.06..296667.33 rows=154 width=187) (actual time=120692.608..121849.571 rows=9 loops=1)
         Hash Cond: (teammember.team_id = team.id)
         ->  Nested Loop  (cost=580.34..44888.22 rows=30892 width=115) (actual time=0.212..0.324 rows=9 loops=1)
               ->  Index Scan using player_tag_key on player  (cost=0.00..8.49 rows=1 width=136) (actual time=0.112..0.117 rows=1 loops=1)
                     Index Cond: ((tag)::text = 'P2822YCPP'::text)
               ->  Bitmap Heap Scan on teammember  (cost=580.34..44493.58 rows=30892 width=115) (actual time=0.082..0.137 rows=9 loops=1)
                     Recheck Cond: ((teammember.player_id)::text = (player.id)::text)
                     ->  Bitmap Index Scan on teammember_player_id_57b471c2_like  (cost=0.00..572.62 rows=30892 width=0) (actual time=0.070..0.070 rows=9 loops=1)
                           Index Cond: ((teammember.player_id)::text = (player.id)::text)
         ->  Hash  (cost=249676.15..249676.15 rows=23646 width=80) (actual time=120056.101..120056.101 rows=4624486 loops=1)
               Buckets: 1024  Batches: 256 (originally 4)  Memory Usage: 1025kB
               ->  Nested Loop  (cost=8.46..249676.15 rows=23646 width=80) (actual time=0.085..107384.058 rows=4624486 loops=1)
                     ->  Seq Scan on battle  (cost=0.00..55176.18 rows=11823 width=84) (actual time=0.033..6167.769 rows=2312243 loops=1)
                           Filter: (("time" >= '2018-08-10 00:00:00+02'::timestamp with time zone) AND ("time" < '2018-09-30 00:00:00+02'::timestamp with time zone))
                     ->  Bitmap Heap Scan on team  (cost=8.46..16.42 rows=2 width=4) (actual time=0.024..0.028 rows=2 loops=2312243)
                           Recheck Cond: ((battle.team_id = team.id) OR (battle.opponentteam_id = team.id))
                           ->  BitmapOr  (cost=8.46..8.46 rows=2 width=0) (actual time=0.018..0.018 rows=0 loops=2312243)
                                 ->  Bitmap Index Scan on team_pkey  (cost=0.00..4.23 rows=1 width=0) (actual time=0.007..0.007 rows=1 loops=2312243)
                                       Index Cond: (battle.team_id = team.id)
                                 ->  Bitmap Index Scan on team_pkey  (cost=0.00..4.23 rows=1 width=0) (actual time=0.006..0.006 rows=1 loops=2312243)
                                       Index Cond: (battle.opponentteam_id = team.id)
 Total runtime: 121849.864 ms
(25 wierszy)

