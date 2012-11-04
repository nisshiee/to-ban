# Create memo table

# --- !Ups

CREATE TABLE memo
(
  date DATE NOT NULL PRIMARY KEY
  ,memo VARCHAR(512)
);


# --- !Downs

DROP TABLE memo;

