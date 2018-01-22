/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnit_Update_DomainMask]')
	   AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnit_Update_DomainMask]
GO

/*===========================================================================*\
  Description:	Updates the Domain_Mask of an item in Collection_Unit. Calls 
		itself until the Current Container key is null.

  Parameters:	@CollectionUnitKey	Key of item to update.
		@DomainMask		Bit to turn on or off.
		@On			0 for tuning bit *OFF*, 1 for *ON*

  Created:	July 2003

  Last revision information:
    $Revision: 9 $
    $Date: 8/12/03 10:22 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnit_Update_DomainMask] 
	@CollectionUnitKey char(16),
	@DomainMask int,
	@On bit
AS
IF @CollectionUnitKey IS NOT NULL
BEGIN
	SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Get current mask.
	\*-------------------------------------------------------------*/
	DECLARE	@CurrentMask int
	SELECT	@CurrentMask = Domain_Mask
	FROM	Collection_Unit
	WHERE	Collection_Unit_Key = @CollectionUnitKey

	/*-------------------------------------------------------------*\
	  Check if current mask has bit *ON* but is currently *OFF*, or
	  has bit *OFF* but is currently *ON*. If either true, proceed.
	\*-------------------------------------------------------------*/
	IF ((@On = 1) AND (@CurrentMask & @DomainMask) <> @DomainMask)	-- Need *ON* but have *OFF*

	BEGIN
		-- Flag to indicate if there is any need to update container.
		-- False by default
		DECLARE	@CheckContainer bit
		SET	@CheckContainer = 0

		/*-------------------------------------------------------------*\
		  Do the update, according to @On switch.
		\*-------------------------------------------------------------*/
		IF @On = 1
		BEGIN
			-- Turn bit *ON* in bitfield.
			UPDATE	Collection_Unit
			SET	Domain_Mask = @CurrentMask | @DomainMask
			WHERE	Collection_Unit_Key = @CollectionUnitKey

			SET	@CheckContainer = 1
		END ELSE BEGIN
			-- Turn bit *OFF* only if there are no other contained items with
			-- that particular bit off. If only one contained item has the bit
			-- still on, don't do anything to the containing item.
			IF NOT EXISTS(
				SELECT	* 
				FROM	Collection_Unit 
				WHERE	Current_Container_Collection_Unit_Key = @CollectionUnitKey
				AND	Domain_Mask & @DomainMask = @DomainMask
				)
			BEGIN
				-- Turn bit *OFF* in bitfield
				UPDATE	Collection_Unit
				SET	Domain_Mask = @CurrentMask & ~ @DomainMask
				WHERE	Collection_Unit_Key = @CollectionUnitKey

				SET	@CheckContainer = 1
			END
		END

		/*-------------------------------------------------------------*\
		  If current item was updated, container must be checked too.
		\*-------------------------------------------------------------*/
		IF @CheckContainer = 1
		BEGIN
			DECLARE	@ContainerKey char(16)
			SELECT	@ContainerKey = Current_Container_Collection_Unit_Key
			FROM	Collection_Unit
			WHERE	Collection_Unit_Key = @CollectionUnitKey
	
			EXECUTE	[dbo].[usp_CollectionUnit_Update_DomainMask] @ContainerKey, @DomainMask, @On
		END

		/*-------------------------------------------------------------*\
		  If it's a specimen, do the collections too.
		\*-------------------------------------------------------------*/
		DECLARE	@CollectionKey char(16)
		SELECT	@CollectionKey = Parent_Collection_Collection_Unit_Key 
		FROM	Specimen_Unit
		WHERE	Collection_Unit_Key = @CollectionUnitKey 

		-- Update the collection's Domain Mask.
		EXECUTE	[dbo].[usp_Collection_Update_DomainMask] @CollectionKey, @DomainMask, @On

		/*-------------------------------------------------------------*\
		  And see if there was an impact on the domain masks stores in 
		  Conservation_Check and Conservation_Job.
		\*-------------------------------------------------------------*/
		EXECUTE usp_ConservationJob_Update_DomainMask @CollectionUnitKey, NULL
		EXECUTE usp_ConservationCheck_Update_DomainMask @CollectionUnitKey, NULL
	END

	SET NOCOUNT OFF
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnit_Update_DomainMask') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnit_Update_DomainMask'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_DomainMask TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_DomainMask TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_DomainMask TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_DomainMask TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_DomainMask TO [Dev - JNCC SQL]
END
GO