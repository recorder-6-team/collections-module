/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationCheck_Update_DomainMask]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationCheck_Update_DomainMask]
GO

/*===========================================================================*\
  Description:	Update the Domain Mask field of all Conservation Checks related
		to either a collection Unit item or a conservation check.
		If no values are provided, all domain masks will be recalculated.
		That could be useful to "repair" the masks in Conservation_Check.

		Use @CollectionUnitKey when item was updated in Collection_Unit.

		Use @ConservationCheckKey when called from trigger on 
		Collection_Unit_Check table. Especially important for DELETE trigger.

  Parameters:	@CollectionUnitKey
		@ConservationCheckKey

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 12/11/03 13:47 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationCheck_Update_DomainMask]
	@CollectionUnitKey char(16) = NULL,
	@ConservationCheckKey char(16) = NULL
AS
	DECLARE	@CheckKey char(16),
		@NewMask int

	/*-------------------------------------------------------------*\
	  Find all related jobs first.
	\*-------------------------------------------------------------*/
	IF @CollectionUnitKey IS NOT NULL
		-- All checks related to a Collection Unit item
		DECLARE curChecks CURSOR LOCAL FAST_FORWARD FOR
			SELECT		DISTINCT CC.Conservation_Check_Key
			FROM		Collection_Unit CU
			INNER JOIN 	Collection_Unit_Check CUC ON CUC.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN	Conservation_Check CC ON CC.Conservation_Check_Key = CUC.Conservation_Check_Key
			WHERE		CU.Collection_Unit_Key = @CollectionUnitKey
	ELSE
	IF @ConservationCheckKey IS NOT NULL
		-- All checks related to a Collection Unit item
		DECLARE curChecks CURSOR LOCAL FAST_FORWARD FOR
			SELECT		DISTINCT CC.Conservation_Check_Key
			FROM		Collection_Unit_Check CUC 
			INNER JOIN	Conservation_Check CC ON CC.Conservation_Check_Key = CUC.Conservation_Check_Key
			WHERE		CUC.Conservation_Check_Key = @ConservationCheckKey 
	ELSE
		-- All checkes related to records in Collection_Unit_Check
		DECLARE curChecks CURSOR LOCAL FAST_FORWARD FOR
			SELECT		DISTINCT CC.Conservation_Check_Key
			FROM		Collection_Unit CU
			INNER JOIN 	Collection_Unit_Check CUC ON CUC.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN	Conservation_Check CC ON CC.Conservation_Check_Key = CUC.Conservation_Check_Key

	/*-------------------------------------------------------------*\
	  Got all jobs. Now update the domain masks.
	\*-------------------------------------------------------------*/
	OPEN curChecks
	FETCH NEXT FROM curChecks INTO @CheckKey
	WHILE @@Fetch_Status = 0
	BEGIN
		-- Recalculate the mask. Need to select first, or we get following error:
		-- "An aggregate may not appear in the set list of an UPDATE statement."
		SELECT		@NewMask = Sum(DISTINCT CU.Domain_Mask)
		FROM		Collection_Unit CU
		INNER JOIN 	Collection_Unit_Check CUC ON CUC.Collection_Unit_Key = CU.Collection_Unit_Key
		INNER JOIN	Conservation_Check CC ON CC.Conservation_Check_Key = CUC.Conservation_Check_Key
		WHERE		CC.Conservation_Check_Key = @CheckKey
	
		-- So do update afterwards
		UPDATE	Conservation_Check
		SET	Domain_Mask = @NewMask
		WHERE	Conservation_Check_Key = @CheckKey
	
		-- Move on
		FETCH NEXT FROM curChecks INTO @CheckKey
	END

	-- Cleanup
	CLOSE curChecks
	DEALLOCATE curChecks
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationCheck_Update_DomainMask') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationCheck_Update_DomainMask'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationCheck_Update_DomainMask TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationCheck_Update_DomainMask TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationCheck_Update_DomainMask TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationCheck_Update_DomainMask TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationCheck_Update_DomainMask TO [Dev - JNCC SQL]
END

GO