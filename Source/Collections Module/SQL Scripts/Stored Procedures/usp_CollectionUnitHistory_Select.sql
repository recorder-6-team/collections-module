/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitHistory_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitHistory_Select]
GO
 
/*===========================================================================*\
  Description:	Returns a collection unit history record.

  Parameters:	@Key	Collection_Unit_History key

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 12/11/03 12:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitHistory_Select]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT 
	CH.Item_Name,
	CH.From_Vague_Date_Start,
	CH.From_Vague_Date_End,
	CH.From_Vague_Date_Type,
	CH.To_Vague_Date_Start,
	CH.To_Vague_Date_End,
	CH.To_Vague_Date_Type,
	CH.Source_Name_Key,
	dbo.ufn_GetFormattedName(CH.Source_Name_Key) AS Source,
	CH.Comment, 
	CH.Custodian,
	CH.Timestamp
FROM Collection_Unit_History CH
WHERE CH.Collection_Unit_History_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitHistory_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitHistory_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Select TO [Dev - JNCC SQL]
END

GO