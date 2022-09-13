CREATE TABLE connection_details(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  bootstrap_host TEXT NOT NULL,
  bootstrap_port INTEGER NOT NULL,
  username TEXT,
  password TEXT,
  is_ssl_on BOOLEAN NOT NULL,
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL,
  last_used_at INTEGER NOT NULL
);
