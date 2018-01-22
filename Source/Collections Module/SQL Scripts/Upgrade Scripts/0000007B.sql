/*============================================================================*\
    TFS 11726/Redmine 1685
    Set the "Include in label" option for all stratigraphic determinations.
\*============================================================================*/
UPDATE      d
SET         d.Include_In_Label          =   1
FROM        dbo.Local_Domain            AS  l
INNER JOIN  dbo.Concept_Group           AS  g
ON          g.Local_Domain_Key          =   l.Local_Domain_Key
INNER JOIN  dbo.Concept                 AS  c
ON          c.Concept_Group_Key         =   g.Concept_Group_Key
INNER JOIN  dbo.Determination           AS  d
ON          d.Concept_Key               =   c.Concept_Key
WHERE       l.Domain_Key                =   'DSS0039400000001' -- the Stratigraphy domain
