# Add status column into Member

# --- !Ups

ALTER TABLE member ADD
  status INT DEFAULT 0 NOT NULL
;

CREATE INDEX member_status_idx ON member (
  status
  ,id
);

# --- !Downs

DROP INDEX member_status_idx;
ALTER TABLE member DROP COLUMN status;
