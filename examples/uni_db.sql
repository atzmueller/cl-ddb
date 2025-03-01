DROP TABLE IF EXISTS attends;
DROP TABLE IF EXISTS requires;
DROP TABLE IF EXISTS tests;
DROP TABLE IF EXISTS Course;
DROP TABLE IF EXISTS Student;
DROP TABLE IF EXISTS Assistant;
DROP TABLE IF EXISTS Professor;

CREATE TABLE Student
(StuNo         INTEGER PRIMARY KEY,
 Name           VARCHAR(30) NOT NULL,
 Semester       INTEGER);

CREATE TABLE Professor
(PerNo         INTEGER PRIMARY KEY,
 Name           VARCHAR(30) NOT NULL,
 Rank           CHAR(2) CHECK (Rank in ('W1', 'W2', 'W3')),
 Room           INTEGER UNIQUE);

CREATE TABLE Assistant
(PerNo         INTEGER PRIMARY KEY,
 Name           VARCHAR(30) NOT NULL,
 Topic     VARCHAR(30),
 Boss           INTEGER,
 FOREIGN KEY    (Boss) REFERENCES Professor(PerNo));

CREATE TABLE Course
(CouNo      INTEGER PRIMARY KEY,
 Title          VARCHAR(30),
 SCH            INTEGER,
 TaughtBy     INTEGER,
 FOREIGN KEY    (TaughtBy) REFERENCES Professor(PerNo));

CREATE TABLE attends
(StuNo         INTEGER REFERENCES Student ON DELETE CASCADE,
 CouNo         INTEGER REFERENCES Course ON DELETE CASCADE,
 PRIMARY KEY    (StuNo, CouNo));

CREATE TABLE requires
(Predecessor   INTEGER REFERENCES Course(CouNo) ON DELETE CASCADE,
 Successor     INTEGER REFERENCES Course(CouNo),
 PRIMARY KEY   (Predecessor, Successor));

CREATE TABLE tests
(StuNo         INTEGER REFERENCES Student ON DELETE CASCADE,
 CouNo         INTEGER REFERENCES Course,
 PerNo         INTEGER REFERENCES Professor,
 Grade         NUMERIC(2,1) CHECK (Grade between 0.7 and 5.0),
 PRIMARY KEY    (StuNo, CouNo));


INSERT INTO Student (StuNo, Name, Semester)
VALUES (24002, 'Xenokrates', 18);

INSERT INTO Student (StuNo, Name, Semester)
VALUES (25403, 'Jonas', 12);

INSERT INTO Student (StuNo, Name, Semester)
VALUES (26120, 'Fichte', 10);

INSERT INTO Student (StuNo, Name, Semester)
VALUES (26830, 'Aristoxenos', 8);

INSERT INTO Student (StuNo, Name, Semester)
VALUES (27550, 'Schopenhauer', 6);

INSERT INTO Student (StuNo, Name, Semester)
VALUES (28106, 'Carnap', 3);

INSERT INTO Student (StuNo, Name, Semester)
VALUES (29120, 'Theophrastos', 2);

INSERT INTO Student (StuNo, Name, Semester)
VALUES (29555, 'Feuerbach', 2);



INSERT INTO Professor (PerNo, Name, Rank, Room)
VALUES (2125, 'Sokrates', 'W3', 226);

INSERT INTO Professor (PerNo, Name, Rank, Room)
VALUES (2126, 'Russel', 'W3', 232);

INSERT INTO Professor (PerNo, Name, Rank, Room)
VALUES (2127, 'Kopernikus', 'W2', 310);

INSERT INTO Professor (PerNo, Name, Rank, Room)
VALUES (2133, 'Popper', 'W2', 052);

INSERT INTO Professor (PerNo, Name, Rank, Room)
VALUES (2134, 'Augustinus', 'W2', 309);

INSERT INTO Professor (PerNo, Name, Rank, Room)
VALUES (2136, 'Curie', 'W3', 036);

INSERT INTO Professor (PerNo, Name, Rank, Room)
VALUES (2137, 'Kant', 'W3', 007);



INSERT INTO Assistant (PerNo, Name, Topic, Boss)
VALUES (3002, 'Platon', 'Theory of Ideas', 2125);

INSERT INTO Assistant (PerNo, Name, Topic, Boss)
VALUES (3003, 'Aristoteles', 'Syllogistics', 2125);

INSERT INTO Assistant (PerNo, Name, Topic, Boss)
VALUES (3004, 'Wittgenstein', 'Theory of Language', 2126);

INSERT INTO Assistant (PerNo, Name, Topic, Boss)
VALUES (3005, 'Rhetikus', 'Planetary Motion', 2127);

INSERT INTO Assistant (PerNo, Name, Topic, Boss)
VALUES (3006, 'Newton', 'Kepler''s Laws', 2127);

INSERT INTO Assistant (PerNo, Name, Topic, Boss)
VALUES (3007, 'Spinoza', 'God and Nature', 2134);



INSERT INTO Course (CouNo, Title, SCH, TaughtBy)
VALUES (5001, 'Fundamental Principles', 4, 2137);

INSERT INTO Course (CouNo, Title, SCH, TaughtBy)
VALUES (5041, 'Ethics', 4, 2125);

INSERT INTO Course (CouNo, Title, SCH, TaughtBy)
VALUES (5043, 'Epistemology', 3, 2126);

INSERT INTO Course (CouNo, Title, SCH, TaughtBy)
VALUES (5049, 'Maeeutics', 2, 2125);

INSERT INTO Course (CouNo, Title, SCH, TaughtBy)
VALUES (4052, 'Logic', 4, 2125);

INSERT INTO Course (CouNo, Title, SCH, TaughtBy)
VALUES (5052, 'Philosophy of Science', 3, 2126);

INSERT INTO Course (CouNo, Title, SCH, TaughtBy)
VALUES (5216, 'Bioethics', 2, 2126);

INSERT INTO Course (CouNo, Title, SCH, TaughtBy)
VALUES (5259, 'The Vienna Circle', 2, 2133);

INSERT INTO Course (CouNo, Title, SCH, TaughtBy)
VALUES (5022, 'Faith and Knowledge', 2, 2134);

INSERT INTO Course (CouNo, Title, SCH, TaughtBy)
VALUES (4630, 'The Three Critiques', 4, 2137);




INSERT INTO attends (StuNo, CouNo)
VALUES (26120, 5001);

INSERT INTO attends (StuNo, CouNo)
VALUES (27550, 5001);

INSERT INTO attends (StuNo, CouNo)
VALUES (27550, 4052);

INSERT INTO attends (StuNo, CouNo)
VALUES (28106, 5041);

INSERT INTO attends (StuNo, CouNo)
VALUES (28106, 5052);

INSERT INTO attends (StuNo, CouNo)
VALUES (28106, 5216);

INSERT INTO attends (StuNo, CouNo)
VALUES (28106, 5259);

INSERT INTO attends (StuNo, CouNo)
VALUES (29120, 5001);

INSERT INTO attends (StuNo, CouNo)
VALUES (29120, 5041);

INSERT INTO attends (StuNo, CouNo)
VALUES (29120, 5049);

INSERT INTO attends (StuNo, CouNo)
VALUES (29555, 5022);

INSERT INTO attends (StuNo, CouNo)
VALUES (25403, 5022);

INSERT INTO attends (StuNo, CouNo)
VALUES (29555, 5001);



INSERT INTO requires (Predecessor, Successor)
VALUES (5001, 5041);

INSERT INTO requires (Predecessor, Successor)
VALUES (5001, 5043);

INSERT INTO requires (Predecessor, Successor)
VALUES (5001, 5049);

INSERT INTO requires (Predecessor, Successor)
VALUES (5041, 5216);

INSERT INTO requires (Predecessor, Successor)
VALUES (5043, 5052);

INSERT INTO requires (Predecessor, Successor)
VALUES (5041, 5052);

INSERT INTO requires (Predecessor, Successor)
VALUES (5052, 5259);



INSERT INTO tests (StuNo, CouNo, PerNo, Grade)
VALUES (28106, 5001, 2126, 1.0);

INSERT INTO tests (StuNo, CouNo, PerNo, Grade)
VALUES (25403, 5041, 2125, 2.0);

INSERT INTO tests (StuNo, CouNo, PerNo, Grade)
VALUES (27550, 4630, 2137, 2.0);
