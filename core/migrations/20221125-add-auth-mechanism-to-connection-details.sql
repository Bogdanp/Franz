ALTER TABLE connection_details ADD COLUMN auth_mechanism TEXT NOT NULL DEFAULT 'plain';
--more--
ALTER TABLE connection_details ADD COLUMN aws_region TEXT;
--more--
ALTER TABLE connection_details ADD COLUMN aws_access_key_id TEXT;
