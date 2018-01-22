/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitTask_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitTask_Insert]
GO
/*===========================================================================*\
  Description:	Inserts a record into the Collection_Unit_Task table.
  Parameters:	@Key  (output)
		@ConservationTaskKey 
		@CollectionUnitKey 
		@SessionID 

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/04 9:51 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitTask_Insert]
	@Key char(16) output,
	@ConservationTaskKey char(16),
	@CollectionUnitKey char(16),
	@SessionID char(16)
	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Collection_Unit_Task', @Key OUTPUT
	
	Insert into Collection_Unit_Task
		(Collection_Unit_Task_Key,
		Conservation_Task_Key,
		Collection_Unit_Key,
		Entered_Session_ID)
	Values
		(@Key,
		@ConservationTaskKey,
		@CollectionUnitKey,
		@SessionID)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitTask_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitTask_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert TO [Dev - JNCC SQL]
END

GO