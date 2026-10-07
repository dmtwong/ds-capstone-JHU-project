-- Schema for ngram word prediction; PostgresSQL 18

-- Drop tables if existed, dependencies may be added so keep this order
DROP TABLE IF EXISTS quadgrams CASCADE;
DROP TABLE IF EXISTS trigrams CASCADE;
DROP TABLE IF EXISTS bigrams CASCADE;

-- bigram, example: ("text", "analytic"), 3
CREATE TABLE bigrams (
    word_1      VARCHAR(50) NOT NULL,
    word_2      VARCHAR(50) NOT NULL,
    frequency   INT NOT NULL DEFAULT 1,
    PRIMARY KEY (word_1, word_2)
);

-- trigran, example: ("text", "analytics", "enable"), 3
CREATE TABLE trigrams (
    word_1      VARCHAR(50) NOT NULL,
    word_2      VARCHAR(50) NOT NULL,
    word_3      VARCHAR(50) NOT NULL,
    frequency   INT NOT NULL DEFAULT 1,
    PRIMARY KEY (word_1, word_2, word_3)
);

-- quadgram, example: ("text", "analytics", "insight"), 3
CREATE TABLE quadgrams (
    word_1      VARCHAR(50) NOT NULL,
    word_2      VARCHAR(50) NOT NULL,
    word_3      VARCHAR(50) NOT NULL,
    word_4      VARCHAR(50) NOT NULL,
    frequency   INT NOT NULL DEFAULT 1,
    PRIMARY KEY (word_1, word_2, word_3, word_4)
);


