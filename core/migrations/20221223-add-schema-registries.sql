CREATE TABLE schema_registries(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  kind TEXT NOT NULL DEFAULT 'confluent',
  url TEXT NOT NULL,
  username TEXT,
  password_id TEXT,
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL
);
ALTER TABLE connection_details
  ADD COLUMN schema_registry_id INTEGER;
