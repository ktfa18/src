SELECT
 T1.C1 AS F1,
 CASE
  WHEN T1.C2+T2.C2<0 THEN 0
  ELSE T1.C2+T2.C2
 END AS F2
FROM
 T1 INNER JOIN T2 ON T1.C1=T2.C1
WHERE
 T1.C2<T2.C2;

CREATE TABLE T1(
 C1 NUMBER(1) NOT NULL,
 C2 NUMBER(1) CHECK(T1.C2<>0));

CREATE TABLE T2(
 C1 NUMBER(1),
 C2 NUMBER(1));
