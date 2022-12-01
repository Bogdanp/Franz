ALTER TABLE connection_details ADD COLUMN auth_mechanism TEXT NOT NULL DEFAULT 'plain';
ALTER TABLE connection_details ADD COLUMN aws_region TEXT;
ALTER TABLE connection_details ADD COLUMN aws_access_key_id TEXT;
