/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreDetailsMemo_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreDetailsMemo_Get]
GO
 
/*===========================================================================*\
  Description:	Returns a the details memo content for the Store general tab

  Parameters:	@Key	Collection_Unit key
							@Output - output.  Note this is limited to 8000 characters.

  Created:	Oct 2003

  Last revision information:
    $Revision: 6 $
    $Date: 6/05/04 14:26 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreDetailsMemo_Get]
	@Key char(16),
	@Output varchar(8000) OUTPUT
AS

SET NOCOUNT ON

--Async query to get details memo content
DECLARE @Value VARCHAR(500) 

DECLARE data_cursor CURSOR FORWARD_ONLY FOR 
	SELECT CT.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS + ' ' + D.Lower_Value AS Value
	FROM Collection_Unit_Data D
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=D.Parameter_Concept_Key
	WHERE D.Collection_Unit_Key=@Key
	AND D.Is_Descriptor=1

OPEN data_cursor

FETCH NEXT FROM data_cursor INTO @Value

SET @Output = ''

WHILE @@Fetch_Status=0
BEGIN
  --New line if required
	IF @Output <> '' 
		SET @Output=@Output+CHAR(13)+CHAR(10)
  SET @Output=@Output+@Value	
	FETCH NEXT FROM data_cursor INTO @Value
END

CLOSE data_cursor
DEALLOCATE data_cursor

DECLARE data_cursor CURSOR FORWARD_ONLY FOR 
	SELECT 
		CASE WHEN D.Upper_Value IS NULL THEN
			D.Lower_Value 
		ELSE
			D.Lower_Value + ' - ' + D.Upper_Value
		END +
    CASE WHEN CTU.Plaintext IS NULL THEN 
			''
		ELSE
			' ' + CTU.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS
		END +
	  ' ' + CTP.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS +
		' (' + D.Applies_To + ')' AS Value
	FROM Collection_Unit_Data D
	LEFT JOIN VW_ConceptTerm CTU ON CTU.Concept_Key=D.Unit_Concept_Key
	INNER JOIN VW_ConceptTerm CTP ON CTP.Concept_Key=D.Parameter_Concept_Key
	WHERE D.Collection_Unit_Key=@Key
	AND D.Is_Descriptor=0

OPEN data_cursor

FETCH NEXT FROM data_cursor INTO @Value

WHILE @@Fetch_Status=0
BEGIN
  --New line if required
	IF @Output <> '' 
		SET @Output=@Output+CHAR(13)+CHAR(10)
  SET @Output=@Output+@Value	
	FETCH NEXT FROM data_cursor INTO @Value
END

CLOSE data_cursor
DEALLOCATE data_cursor
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreDetailsMemo_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreDetailsMemo_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [Dev - JNCC SQL]
END

GO