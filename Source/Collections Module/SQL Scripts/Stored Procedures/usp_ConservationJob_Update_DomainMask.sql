/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJob_Update_DomainMask]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJob_Update_DomainMask]
GO

/*===========================================================================*\
  Description:	Update the Domain Mask field of all Conservation Jobs related
		to either a collection Unit item or a conservatio task.
		If no values are provided, all domain masks will be recalculated.
		That could be useful to "repair" the masks inConservation_Job.

		Use @CollectionUnitKey when item was updated in Collection_Unit.

		Use @ConservationTaskKey when called from trigger on 
		Collection_Unit_Task table. Especially important for DELETE trigger.

  Parameters:	@CollectionUnitKey
		@ConservationTaskKey

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 3/01/05 14:58 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJob_Update_DomainMask]
	@CollectionUnitKey char(16) = NULL,
	@ConservationTaskKey char(16) = NULL
AS
	DECLARE	@JobKey char(16),
		@NewMask int

	/*-------------------------------------------------------------*\
	  Find all related jobs first.
	\*-------------------------------------------------------------*/
	IF @CollectionUnitKey IS NOT NULL
		-- All jobs related to a Collection Unit item
		DECLARE curJobs CURSOR LOCAL FAST_FORWARD FOR
			SELECT		DISTINCT CJ.Conservation_Job_Key
			FROM		Collection_Unit CU
			INNER JOIN 	Collection_Unit_Task CUT ON CUT.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN	Conservation_Task CT ON CT.Conservation_Task_Key = CUT.Conservation_Task_Key
			INNER JOIN	Conservation_Job CJ ON CJ.Conservation_Job_Key = CT.Conservation_Job_Key
			WHERE		CU.Collection_Unit_Key = @CollectionUnitKey
	ELSE 
	IF @ConservationTaskKey IS NOT NULL
		-- All jobs related to a Conservation Task. 
		DECLARE curJobs CURSOR LOCAL FAST_FORWARD FOR
			SELECT		DISTINCT CJ.Conservation_Job_Key
			FROM		Conservation_Task CT 
			INNER JOIN	Conservation_Job CJ ON CJ.Conservation_Job_Key = CT.Conservation_Job_Key
			WHERE		CT.Conservation_Task_Key = @ConservationTaskKey
	ELSE
		-- All jobs related to records in Collection_Unit_Task
		DECLARE curJobs CURSOR LOCAL FAST_FORWARD FOR
			SELECT		DISTINCT CJ.Conservation_Job_Key
			FROM		Collection_Unit CU
			INNER JOIN 	Collection_Unit_Task CUT ON CUT.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN	Conservation_Task CT ON CT.Conservation_Task_Key = CUT.Conservation_Task_Key
			INNER JOIN	Conservation_Job CJ ON CJ.Conservation_Job_Key = CT.Conservation_Job_Key
	
	
	/*-------------------------------------------------------------*\
	  Got all jobs. Now update the domain masks.
	\*-------------------------------------------------------------*/
	OPEN curJobs
	FETCH NEXT FROM curJobs INTO @JobKey
	WHILE @@Fetch_Status = 0
	BEGIN
		-- Recalculate the mask. Need to select first, or we get following error:
		-- "An aggregate may not appear in the set list of an UPDATE statement."
		SELECT		@NewMask = Sum(DISTINCT CU.Domain_Mask)
		FROM		Collection_Unit CU
		INNER JOIN 	Collection_Unit_Task CUT ON CUT.Collection_Unit_Key = CU.Collection_Unit_Key
		INNER JOIN	Conservation_Task CT ON CT.Conservation_Task_Key = CUT.Conservation_Task_Key
		INNER JOIN	Conservation_Job CJ ON CJ.Conservation_Job_Key = CT.Conservation_Job_Key
		WHERE		CJ.Conservation_Job_Key = @JobKey
	
		-- So do update afterwards
		UPDATE	Conservation_Job
		SET	Domain_Mask = IsNull(@NewMask, 0)
		WHERE	Conservation_Job_Key = @JobKey
	
		-- Move on
		FETCH NEXT FROM curJobs INTO @JobKey
	END

	-- Cleanup
	CLOSE curJobs
	DEALLOCATE curJobs
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJob_Update_DomainMask') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJob_Update_DomainMask'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Update_DomainMask TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Update_DomainMask TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Update_DomainMask TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Update_DomainMask TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Update_DomainMask TO [Dev - JNCC SQL]
END

GO