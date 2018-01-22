/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MetadataAndType_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MetadataAndType_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of metadata types for a given table name, and
		any data associated with the specified record key. Metadata 
		types are returned even if there is no data for the record.

  Parameters:	@TableName	The name of the table to filter the types on.
		@RecordKey	The key of the record for which the data is
				requested.

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MetadataAndType_Select]
	@TableName varchar(50),
	@RecordKey char(16) = NULL
AS
	SELECT		MT.Metadata_Type_Key, 
			MT.Item_Name, 
			MT.[Description],
			M.Metadata_Key,
			M.[Text],
			M.Custodian,
			M.[Timestamp]
	FROM		Metadata_Type MT
	LEFT JOIN	Metadata M ON M.Metadata_Type_Key = MT.Metadata_Type_Key
			AND M.Record_Key = @RecordKey
	WHERE		MT.Table_Name = @TableName
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MetadataAndType_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MetadataAndType_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MetadataAndType_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MetadataAndType_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MetadataAndType_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MetadataAndType_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MetadataAndType_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MetadataAndType_Select TO [Dev - JNCC SQL]
END

GO