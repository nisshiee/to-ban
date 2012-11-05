# Add status column into Member

# --- !Ups

ALTER TABLE member ADD
  color INT DEFAULT 0 NOT NULL
;

# --- !Downs

ALTER TABLE member DROP COLUMN color;
