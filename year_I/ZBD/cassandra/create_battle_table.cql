CREATE TABLE clashroyale_battles (
        id uuid,
	player_tag text,
	utc_time timestamp,

	clan_tag text,

	arena_id tinyint,
	team_size tinyint,

        win tinyint,
        loss tinyint,
        draw tinyint,

	type varchar,
	deck_type varchar,
	deck list<frozen <tuple<int, text, smallint> > >,
	PRIMARY KEY((player_tag), utc_time, id)
);
