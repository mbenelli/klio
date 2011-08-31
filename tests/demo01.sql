CREATE TABLE history(
		timestamp INTEGER PRIMARY KEY ASC,

		channel1 REAL,
		target_setpoint1 REAL,
		current_setpoint1 REAL,
		slope1 REAL,

		channel2 REAL,
		target_setpoint2 REAL,
		current_setpoint2 REAL,
		slope2 REAL,

		channel3 REAL,
		target_setpoint3 REAL,
		current_setpoint3 REAL,
		slope3 REAL,

		channel4 REAL,
		target_setpoint4 REAL,
		current_setpoint4 REAL,
		slope4 REAL,

		channel5 REAL,
		target_setpoint5 REAL,
		current_setpoint5 REAL,
		slope5 REAL,

		channel6 REAL,
		target_setpoint6 REAL,
		current_setpoint6 REAL,
		slope6 REAL,

		dedicated,
		auxiliaries);


CREATE TABLE alarms(
       id TEXT,
       start INTEGER,
       end INTEGER);
