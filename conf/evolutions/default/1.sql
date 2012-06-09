# CREATE Task, Member, Toban Table

# --- !Ups

CREATE TABLE task (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY
  ,name VARCHAR(32) NOT NULL
);

CREATE TABLE member (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY
  ,name VARCHAR(32) NOT NULL
);

CREATE TABLE toban (
  task_id INT NOT NULL
  ,date DATE NOT NULL
  ,member_id INT NOT NULL
);

CREATE UNIQUE INDEX toban_idx ON toban (
  task_id
  ,date
);


# --- !Downs

DROP INDEX toban_idx;
DROP TABLE toban;
DROP TABLE member;
DROP TABLE task;

