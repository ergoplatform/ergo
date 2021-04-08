create table blocks
(
	id varchar not null
		constraint blocks_pk
			primary key,
	height int not null,
	cost int,
	validation_time decimal
);

create unique index blocks_height_uindex
	on blocks (height);

create unique index blocks_id_uindex
	on blocks (id);

create table transactions
(
	id varchar not null
		constraint transactions_pk
			primary key,
	block_id varchar not null,
	cost int,
	verification_time decimal
);

create unique index transactions_id_uindex
	on transactions (id);

create table inputs
(
	id varchar not null
		constraint inputs_pk
			primary key,
	tx_id varchar not null,
	cost int,
	verification_time decimal
);

create unique index inputs_id_uindex
	on inputs (id);

