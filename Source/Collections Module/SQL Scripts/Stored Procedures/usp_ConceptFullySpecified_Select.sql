If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConceptFullySpecified_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConceptFullySpecified_Select]
GO

CREATE PROCEDURE [dbo].[usp_ConceptFullySpecified_Select] 
@ItemKey char(16),
@Output varchar(500) output

AS

--  DESCRIPTION
--  Returns a fully specified concept for a concept_key.  E.g. Name + Author (list preferred name + author).
--
--  PARAMETERS
--  NAME					DESCRIPTION
--	@ItemKey 			Key of the concept
--
--
--  CREATED:    		2003-08-24
--
SET NOCOUNT ON

-- Get selected term
DECLARE @ListPreferred bit

SELECT @Output = C.Published_Term,
  @ListPreferred = C.List_Preferred
FROM Concept C
WHERE C.Concept_Key=@ItemKey


-- If this is not the list preferred, then find that and append it to the output
IF @ListPreferred=0 BEGIN
	SELECT @Output = @Output + C2.Published_Term
	FROM Concept C1
  INNER JOIN Concept C2 on C2.Meaning_Key=C1.Meaning_Key
	WHERE C1.Concept_Key=@ItemKey
    AND C2.Concept_Group_Key=C1.Concept_Group_Key
    AND C2.List_Preferred=1  
END


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptFullySpecified_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptFullySpecified_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptFullySpecified_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecified_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecified_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecified_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecified_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptFullySpecified_Select TO [Dev - JNCC SQL]
END

GO
