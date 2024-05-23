CREATE TABLE exampleViewTable
(
    `a1` UInt32,
    `a2` UInt32,
    `a3` String
)
ENGINE = MergeTree
PARTITION BY ()
ORDER BY ();

CREATE VIEW exampleParametrizedView AS
SELECT *
FROM exampleViewTable
WHERE (a1 > {a1MoreThan:UInt32}) AND  (a2 < {a2LessThan:UInt32});

INSERT INTO exampleViewTable (*) VALUES
(1, 9, 'a'),
(3, 7, 'b'),
(5, 5, 'c');
