If EXISTS (
	SELECT * FROM SysObjects 
	WHERE Id = OBJECT_ID(N'[dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen]') 
	AND	  OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen]
GO

/*===========================================================================*\
  Description:	update the domain mask when user moving a local domain or 
				concept group results in the node falling inside a different domain branch, 
				then it is necessary to recalulate the entire domain structure of the 
				collection database for all relate date

  Parameters:
	@Collection_Unit_Key:	the unique key for collection unit
	@OldDomainMask:			the old domain mask 
	@NewDomainMask:			the new domain mask

 Created:	Qing Sun	28/11/2008

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] 
	@SpecimenCollectionUnitKey		Char(16),
	@OldDomainMask					INT,
	@NewDomainMask					INT		
AS

	SET NOCOUNT ON

	DECLARE	@ContainerKey			CHAR(16),
			@ParentCollectionKey	CHAR(16),
			@SpecimenMask			INT,
			@CurrentDomainMask		INT,
			@Collection_Key			CHAR(16)
		
		-- get current domain mask
	SELECT	@CurrentDomainMask		= Domain_Mask 
	FROM	Collection_Unit 
	WHERE	Collection_Unit_Key		= @SpecimenCollectionUnitKey	
	SET		@NewDomainMask			= @NewDomainMask	-	(@OldDomainMask & @NewDomainMask)
	SET		@OldDomainMask			= @OldDomainMask	-	(@OldDomainMask & @NewDomainMask)

	SET @SpecimenMask			= @CurrentDomainMask | @NewDomainMask  --> @SpecimenMask = SpecimenMask OR NewDomainMask

	IF @OldDomainMask <> 0
	BEGIN
		IF NOT EXISTS(
			SELECT		1 
			FROM		Collection_Unit 
			WHERE		Current_Container_Collection_Unit_Key			= @SpecimenCollectionUnitKey
			AND			Domain_Mask & @OldDomainMask					= @OldDomainMask
			UNION
			SELECT		1
			FROM		Specimen_Unit	S
			JOIN		Collection_Unit CU ON CU.Collection_Unit_Key	= S.Collection_Unit_Key
			WHERE		S.Parent_Collection_Collection_Unit_Key			= @SpecimenCollectionUnitKey
			AND			CU.Domain_Mask & @OldDomainMask					= @OldDomainMask
		)	
			SET @SpecimenMask = @SpecimenMask & ~ @OldDomainMask   ---> @SpecimenMask = SpecimenMask AND (NOT OldDomainMask)
		
	END
	IF  @SpecimenMask <> @CurrentDomainMask
	BEGIN
		UPDATE	Collection_Unit
		SET		Domain_Mask			= @SpecimenMask
		WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey
		
		SELECT	@ContainerKey			= CU.Current_Container_Collection_Unit_Key,
				@ParentCollectionKey	= SU.Parent_Collection_Collection_Unit_Key
		FROM	Collection_Unit CU
		JOIN	Specimen_Unit SU	ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
		WHERE	CU.Collection_Unit_Key	= @SpecimenCollectionUnitKey
		
		EXECUTE	[dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] @ContainerKey, @OldDomainMask,@NewDomainMask
		EXECUTE	[dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] @ParentCollectionKey, @OldDomainMask,@NewDomainMask
			/*-------------------------------------------------------------*\
		  And see if there was an impact on the domain masks stores in 
		  Conservation_Check and Conservation_Job.
		\*-------------------------------------------------------------*/
		EXECUTE [dbo].[usp_ConservationJob_Update_DomainMask]	@SpecimenCollectionUnitKey, NULL
		EXECUTE [dbo].[usp_ConservationCheck_Update_DomainMask] @SpecimenCollectionUnitKey, NULL
	END	
	SET NOCOUNT OFF

GO


IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('[dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen]') AND SysStat & 0xf = 4)
BEGIN
		PRINT 'Setting up security on procedure [usp_CollectionUnit_Update_DomainMask_ForSpecimen]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] TO [Dev - JNCC SQL]
END

GO