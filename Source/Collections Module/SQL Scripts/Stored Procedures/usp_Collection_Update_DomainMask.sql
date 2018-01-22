/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Collection_Update_DomainMask]') 
	   AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Collection_Update_DomainMask]
GO

/*===========================================================================*\
  Description:	Updates the Domain_Mask of a Collection.
		Calls itself until a collection's parent key is null.

  Parameters:	@CollectionKey	Key of Collection to update.
		@DomainMask	Bit to turn on or off.
		@On		0 for tuning bit *OFF*, 1 for *ON*

  Created:	July 2003

  Last revision information:
    $Revision: 5 $
    $Date: 12/11/03 12:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collection_Update_DomainMask]
	@CollectionKey char(16),
	@DomainMask int,
	@On bit
AS
IF @CollectionKey IS NOT NULL
BEGIN
	SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	| Get current mask 						|
	\*-------------------------------------------------------------*/
	DECLARE	@CurrentMask int
	SELECT	@CurrentMask = Domain_Mask
	FROM	Collection_Unit
	WHERE	Collection_Unit_Key = @CollectionKey

	/*-------------------------------------------------------------*\
	| Check if current mask has bit *ON* but is currently *OFF*, or |
	| has bit *OFF* but is currently *ON*. If either true, proceed.	|
	\*-------------------------------------------------------------*/
	IF ((@On = 1) AND (@CurrentMask & @DomainMask) <> @DomainMask)	-- Need *ON* but have *OFF*
	OR ((@On = 0) AND (@CurrentMask & @DomainMask) = @DomainMask)	-- Need *OFF* but have *ON*
	BEGIN
		-- Flag to indicate if there is any need to update parent collection.
		-- False by default
		DECLARE	@CheckParent bit
		SET	@CheckParent = 0

		/*-------------------------------------------------------------*\
		| Do the update, according to @On switch.			|
		\*-------------------------------------------------------------*/
		IF @On = 1
		BEGIN
			-- Turn bit *ON* in bitfield -> New_Mask = Current_Mask OR Domain_Mask
			UPDATE	Collection_Unit
			SET	Domain_Mask = @CurrentMask | @DomainMask
			WHERE	Collection_Unit_Key = @CollectionKey

			SET	@CheckParent = 1
		END ELSE BEGIN
			-- Turn bit *OFF* only if there are no other contained items with
			-- that particular bit off. If only one contained item has the bit
			-- still on, don't do anything to the containing item.
			IF NOT EXISTS(
				-- Unions need to have the same number of fields in select
				SELECT		1
				FROM		Collection C
				INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = C.Collection_Unit_Key
				WHERE		C.Parent_Collection_Collection_Unit_Key = @CollectionKey
				AND		CU.Domain_Mask & @DomainMask = @DomainMask
				UNION
				SELECT		1
				FROM		Specimen_Unit S
				INNER JOIN	Collection_Unit CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
				WHERE		S.Parent_Collection_Collection_Unit_Key = @CollectionKey
				AND		CU.Domain_Mask & @DomainMask = @DomainMask
			)
			BEGIN
				-- Turn bit *OFF* in bitfield  -> New_Mask = Current_Mask AND (NOT Domain_Mask)
				UPDATE	Collection_Unit
				SET	Domain_Mask = @CurrentMask & ~ @DomainMask
				WHERE	Collection_Unit_Key = @CollectionKey

				SET	@CheckParent = 1
			END
		END

		/*-------------------------------------------------------------*\
		| If current item was updated, container must be checked too.	|
		\*-------------------------------------------------------------*/
		IF @CheckParent = 1
		BEGIN
			DECLARE	@ParentKey char(16)
			SELECT	@ParentKey = Parent_Collection_Collection_Unit_Key
			FROM	Collection
			WHERE	Collection_Unit_Key = @CollectionKey
	
			EXECUTE	[dbo].[usp_Collection_Update_DomainMask] @ParentKey, @DomainMask, @On
		END
	END

	SET NOCOUNT OFF
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Update_DomainMask') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Update_DomainMask'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Update_DomainMask TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Update_DomainMask TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Update_DomainMask TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Update_DomainMask TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Update_DomainMask TO [Dev - JNCC SQL]
END

GO