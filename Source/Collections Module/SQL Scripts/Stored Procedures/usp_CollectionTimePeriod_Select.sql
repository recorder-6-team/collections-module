/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionTimePeriod_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionTimePeriod_Select]
GO
 
/*===========================================================================*\
  Description:	Returns a collection record.

  Parameters:	@Key	Collection key

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 12:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionTimePeriod_Select]
	@Key char(16)
AS

SET NOCOUNT ON
	SELECT	Collation_From_Vague_Date_Start,
		Collation_From_Vague_Date_End,
		Collation_From_Vague_Date_Type,
		Collation_To_Vague_Date_Start,
		Collation_To_Vague_Date_End,
		Collation_To_Vague_Date_Type,
		Gather_From_Vague_Date_Start,
		Gather_From_Vague_Date_End,
		Gather_From_Vague_Date_Type,
		Gather_To_Vague_Date_Start,
		Gather_To_Vague_Date_End,
		Gather_To_Vague_Date_Type,
		Historical_Period_From,
		Historical_Period_To
	FROM	Collection
	WHERE	Collection_Unit_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionTimePeriod_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionTimePeriod_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionTimePeriod_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionTimePeriod_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionTimePeriod_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionTimePeriod_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionTimePeriod_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionTimePeriod_Select TO [Dev - JNCC SQL]
END

GO
