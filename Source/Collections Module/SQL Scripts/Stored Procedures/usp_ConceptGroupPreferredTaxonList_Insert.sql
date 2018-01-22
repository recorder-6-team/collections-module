/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'dbo.usp_ConceptGroupPreferredTaxonList_Insert')
	   AND    Type = 'P')
	DROP PROCEDURE dbo.usp_ConceptGroupPreferredTaxonList_Insert
GO

/*===========================================================================*\
  Description:	Adds a preferred taxon list for a concept group

  Parameters:	@ConceptGroupKey CHAR(16),
				@TaxonListKey CHAR(16)

  Created:		May 2011

  Last revision information:
	$Revision: 1 $
	$Date: 30/05/11 14:28 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_ConceptGroupPreferredTaxonList_Insert
	@ConceptGroupKey CHAR(16),
	@TaxonListKey CHAR(16)
AS
	DECLARE @Priority INT
	SELECT @Priority = MAX(Priority)
	FROM Concept_Group_Preferred_Taxon_List
	WHERE Concept_Group_Key = @ConceptGroupKey

	SELECT @Priority = CASE	WHEN @Priority IS NULL THEN 0
						ELSE @Priority + 1 END

	INSERT INTO Concept_Group_Preferred_Taxon_List(
		Concept_Group_Key,
		Priority,
		Taxon_List_Key)
	VALUES (
		@ConceptGroupKey,
		@Priority,
		@TaxonListKey)
		
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupPreferredTaxonList_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupPreferredTaxonList_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupPreferredTaxonList_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupPreferredTaxonList_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupPreferredTaxonList_Insert TO [Dev - JNCC SQL]
END
GO