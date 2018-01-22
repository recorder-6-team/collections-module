/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnit_Update_ForSpecimen]')
	   AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnit_Update_ForSpecimen]
GO

/*===========================================================================*\
  Description:	Updates a record in the Specimen_Unit table so that there is a 
		join between the parent Collection Unit node and the child Specimen node.

  Parameters:	@ParentKey 	The key of the top level (parent) Specimen node.
		@ChildKey	The key of the (child) Collection Unit node. 

  Created:	September 2003

  Last revision information:
    $Revision: 6 $
    $Date: 14/04/04 17:53 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnit_Update_ForSpecimen] 
	@ParentKey char(16),
	@ChildKey char(16),
	@JoinKey char(16) OUTPUT
AS
	SET NOCOUNT ON

	SET @JoinKey = @ChildKey

	DECLARE @ExistingCollectionKey char(16),
		@SpecimenMask int

	-- Initialise variables.
	SELECT		@ExistingCollectionKey = SU.Parent_Collection_Collection_Unit_Key
	FROM		Specimen_Unit SU
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
	WHERE		SU.Collection_Unit_Key = @ParentKey

	-- Retrieve the mask of the preferred concept for specimen.
	EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @ParentKey, @SpecimenMask OUTPUT

	BEGIN TRANSACTION

		UPDATE	Specimen_Unit
		SET 	Parent_Collection_Collection_Unit_Key = @ChildKey
		WHERE	Collection_Unit_Key = @ParentKey
		
		-- Update the "old" collection mask, switch bits OFF
		EXECUTE	usp_Collection_Update_DomainMask @ExistingCollectionKey, @SpecimenMask, 0

		-- Update the "new" collection mask, switch bits ON
		EXECUTE	usp_Collection_Update_DomainMask @ChildKey, @SpecimenMask, 1

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnit_Update_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnit_Update_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForSpecimen TO [Dev - JNCC SQL]
END
GO