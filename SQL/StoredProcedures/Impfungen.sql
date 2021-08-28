use RKI;

delimiter //

DROP PROCEDURE IF EXISTS ImpfungenAlter //

CREATE PROCEDURE ImpfungenAlter (Von INT, Bis INT)

BEGIN

SELECT 
    CASE WHEN week(Impfdatum,3)=53 THEN 2020 ELSE year(Impfdatum) END AS Jahr
    , week(Impfdatum,3) AS Kw 
    , AlterVon
    , AlterBis
    , sum(Anzahl) as Anzahl
FROM Impfungen 
WHERE
    AlterVon >= Von
    and AlterBis <= Bis
    and Impfschutz = 2
GROUP BY
    Jahr
    , Kw
    , AlterVon
ORDER BY
    Jahr
    , KW
    , AlterVon
;
end

//

delimiter ;
