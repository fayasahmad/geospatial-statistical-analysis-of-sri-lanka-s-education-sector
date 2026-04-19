-- 1) Final table for MOHE universities (geometry in Sri Lanka Grid / Kandawala EPSG:5235)
DROP TABLE IF EXISTS mohe CASCADE;
CREATE TABLE mohe (
  uni_id   SERIAL PRIMARY KEY,
  name     TEXT NOT NULL UNIQUE,
  district TEXT,
  geom     geometry(Point, 5235) NOT NULL
);
CREATE INDEX universities_gix ON mohe USING GIST (geom);

-- 2) View exposing Lat/Long (EPSG:4326) for labeling
DROP VIEW IF EXISTS mohe.v_universities_4326;
CREATE VIEW mohe.v_universities_4326 AS
SELECT
  uni_id,
  name,
  district,
  ST_Y(ST_Transform(geom,4326))::numeric(9,6) AS latitude,
  ST_X(ST_Transform(geom,4326))::numeric(9,6) AS longitude,
  ST_Transform(geom,4326) AS geom_4326
FROM mohe;
