/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConceptFullySpecifiedWithCG_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConceptFullySpecifiedWithCG_Get]
GO


/*===========================================================================*\
  Description:	Returns a fully specified concept for a concept_key.  
		E.g. Name + Author (list preferred name + author).

  Parameters:	@Key

  Created:	2003-08-24

  Last revision information:
    $Revision: 2 $
    $Date: 3/08/11 14:42 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptFullySpecifiedWithCG_Get] 
@ItemKey char(16),
@Output varchar(500) output

AS

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

-- Add the concept group name
SELECT @Output = @Output + ' - ' + CG.Item_Name
FROM Concept C 
INNER JOIN Concept_Group CG
ON CG.Concept_Group_Key=C.Concept_Group_Key
WHERE C.Concept_Key=@ItemKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptFullySpecifiedWithCG_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptFullySpecifiedWithCG_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptFullySpecifiedWithCG_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecifiedWithCG_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecifiedWithCG_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecifiedWithCG_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecifiedWithCG_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptFullySpecifiedWithCG_Get TO [Dev - JNCC SQL]
END

GO
