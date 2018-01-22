/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE  Id = Object_Id(N'[dbo].[usp_Specimen_Update_ForStore]') AND Type = 'P')
	DROP PROCEDURE [dbo].[usp_Specimen_Update_ForStore]
GO

/*===========================================================================*\
  Description:	Updates link between Specimen and its Store.

  Parameters:
	@ParentKey 				Store Key
	@ChildKey 				Specimen Key
	@UpdateUsualLocation	Flag to indicate whether usual location also gets updated.

  Created:	October 2003

  Last revision information:
    $Revision: 7 $
    $Date: 3/09/08 14:27 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Update_ForStore] 
	@ParentKey 				CHAR(16),
	@ChildKey 				CHAR(16),
	@UpdateUsualLocation	BIT,
	@JoinKey 				CHAR(16) OUTPUT
AS
	SET NOCOUNT ON

	SET @JoinKey = @ChildKey

	DECLARE @ExistingContainerKey 	CHAR(16),
			@SpecimenMask 			INT

	-- Initialise variables.
	SELECT		@ExistingContainerKey = CU.Current_Container_Collection_Unit_Key
	FROM		Specimen_Unit SU
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
	WHERE		SU.Collection_Unit_Key = @ChildKey

	-- Retrieve the mask of the preferred concept for specimen.
	EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @ParentKey, @SpecimenMask OUTPUT

	BEGIN TRANSACTION

		UPDATE	Collection_Unit
		SET		Current_Container_Collection_Unit_Key = @ParentKey
		WHERE	Collection_Unit_Key = @ChildKey

		IF @UpdateUsualLocation = 1
			UPDATE	Collection_Unit
			SET		Usual_Container_Collection_Unit_Key = @ParentKey
			WHERE	Collection_Unit_Key = @ChildKey

		-- Update the "old" container mask, switch bits OFF.
		EXECUTE	usp_CollectionUnit_Update_DomainMask @ExistingContainerKey, @SpecimenMask, 0

		-- Update the "new" container mask, switch bits ON.
		EXECUTE	usp_CollectionUnit_Update_DomainMask @ParentKey, @SpecimenMask, 1

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Update_ForStore') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Specimen_Update_ForStore'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimen_Update_ForStore TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Update_ForStore TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Update_ForStore TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Update_ForStore TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimen_Update_ForStore TO [Dev - JNCC SQL]
END
GO