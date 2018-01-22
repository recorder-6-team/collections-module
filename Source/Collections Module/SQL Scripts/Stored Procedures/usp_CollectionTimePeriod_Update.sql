/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionTimePeriod_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionTimePeriod_Update]
GO
 
/*===========================================================================*\
  Description:	Returns a collection record.

  Parameters:	@Key			Collection key
		@CollationFromStart
		@CollationFromEnd
		@CollationFromType
		@CollationToStart
		@CollationToEnd
		@CollationToType
		@GatherFromStart
		@GatherFromEnd
		@GatherFromType
		@GatherToStart
		@GatherToEnd
		@GatherToType
		@HistoricalPeriodFrom
		@HistoricalPeriodTo

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 12:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionTimePeriod_Update]
	@Key char(16),
	@CollationFromStart int,
	@CollationFromEnd int,
	@CollationFromType varchar(2),
	@CollationToStart int,
	@CollationToEnd int,
	@CollationToType varchar(2),
	@GatherFromStart int,
	@GatherFromEnd int,
	@GatherFromType varchar(2),
	@GatherToStart int,
	@GatherToEnd int,
	@GatherToType varchar(2),
	@HistoricalPeriodFrom varchar(30),
	@HistoricalPeriodTo varchar(30)
AS

SET NOCOUNT ON
	UPDATE	Collection
	SET	Collation_From_Vague_Date_Start = @CollationFromStart,
		Collation_From_Vague_Date_End = @CollationFromEnd,
		Collation_From_Vague_Date_Type = @CollationFromType,
		Collation_To_Vague_Date_Start = @CollationToStart,
		Collation_To_Vague_Date_End = @CollationToEnd,
		Collation_To_Vague_Date_Type = @CollationToType,
		Gather_From_Vague_Date_Start = @GatherFromStart,
		Gather_From_Vague_Date_End = @GatherFromEnd,
		Gather_From_Vague_Date_Type = @GatherFromType,
		Gather_To_Vague_Date_Start = @GatherToStart,
		Gather_To_Vague_Date_End = @GatherToEnd,
		Gather_To_Vague_Date_Type = @GatherToType,
		Historical_Period_From = @HistoricalPeriodFrom,
		Historical_Period_To = @HistoricalPeriodTo
	WHERE	Collection_Unit_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionTimePeriod_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionTimePeriod_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionTimePeriod_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionTimePeriod_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionTimePeriod_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionTimePeriod_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionTimePeriod_Update TO [Dev - JNCC SQL]
END

GO
