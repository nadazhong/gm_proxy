CREATE TABLE IF NOT EXISTS users (
  user_id INTEGER UNSIGNED NOT NULL,
  open_udid VARCHAR(128) DEFAULT '',
  user_name VARCHAR(64) DEFAULT '',
  server_name VARCHAR(255) DEFAULT '',
  server_id INTEGER DEFAULT 0,
  PRIMARY KEY(user_id)
) ENGINE = MYISAM;