/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'dbo.usp_ConceptGroupPreferredTaxonList_UpdatePriority')
	   AND    Type = 'P')
	DROP PROCEDURE dbo.usp_ConceptGroupPreferredTaxonList_UpdatePriority
GO

/*===========================================================================*\
  Description:	Update priority of a preferred taxon list

  Parameters:	@ConceptGroupKey CHAR(16),
				@TaxonListKey CHAR(16)

  Created:		May 2011

  Last revision information:
	$Revision: 2 $
	$Date: 9/06/11 14:41 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_ConceptGroupPreferredTaxonList_UpdatePriority
	@ConceptGroupKey CHAR(16),
	@TaxonListKey CHAR(16),
	@MoveUp BIT
AS
	DECLARE @ReplacedPriority INT,
			@SelectedPriority INT
	
	SELECT @SelectedPriority = Priority							
	FROM Concept_Group_Preferred_Taxon_List
	WHERE Concept_Group_Key = @ConceptGroupKey
	AND Taxon_List_Key = @TaxonListKey	

	IF @MoveUp = 1
	BEGIN
		SELECT @ReplacedPriority = MAX(Priority)
		FROM Concept_Group_Preferred_Taxon_List
		WHERE Concept_Group_Key = @ConceptGroupKey
		AND Priority < @SelectedPriority
	END
	ELSE BEGIN
		SELECT @ReplacedPriority = MIN(Priority)
		FROM Concept_Group_Preferred_Taxon_List
		WHERE Concept_Group_Key = @ConceptGroupKey
		AND Priority > @SelectedPriority
	END

	UPDATE Concept_Group_Preferred_Taxon_List
	SET Priority =	@SelectedPriority
	WHERE Concept_Group_Key = @ConceptGroupKey
	AND Priority = @ReplacedPriority

	UPDATE Concept_Group_Preferred_Taxon_List
	SET Priority = @ReplacedPriority
	WHERE Concept_Group_Key = @ConceptGroupKey
	AND Taxon_List_Key = @TaxonListKey
	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupPreferredTaxonList_UpdatePriority') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupPreferredTaxonList_UpdatePriority'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupPreferredTaxonList_UpdatePriority TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupPreferredTaxonList_UpdatePriority TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupPreferredTaxonList_UpdatePriority TO [Dev - JNCC SQL]
END
GO