/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESession_SearchAndReplace]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESession_SearchAndReplace]
GO

/*===========================================================================*\
  Description:	Searches and replaces a string in a given template field for
	a quick entry session.  Returns a list of data item keys that were 
	updated

  Parameters:	@QESessionKey
			@TemplateFieldKey
			@SearchText
			@ReplaceText			

  Created:	Jan 2006

  Last revision information:
    $Revision: 2 $
    $Date: 5/01/06 9:49 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESession_SearchAndReplace]
@QESessionKey CHAR(16),
@TemplateFieldKey CHAR(16),
@SearchText VARCHAR(100),
@ReplaceText VARCHAR(100)
AS
	-- Return a results set that lists the items that are updated
	SELECT QE_Data_Item_Key
	FROM QE_Data_Item DI
	INNER JOIN QE_Data_Row DR
		ON DR.QE_Data_Row_Key=DI.QE_Data_Row_Key
		AND DR.QE_Session_Key=@QESessionKey
		AND DR.Processed=0
	WHERE DI.QE_Template_Field_Key=@TemplateFieldKey
	AND DI.Data_Display LIKE '%' + @SearchText + '%'

	-- Now perform the udpate
	UPDATE DI
	SET 	DI.Data_Display=REPLACE(DI.Data_Display, @SearchText, @ReplaceText),
		DI.Data_Value=REPLACE(DI.Data_Value, @SearchText, @ReplaceText)
	FROM QE_Data_Item DI
	INNER JOIN QE_Data_Row DR
		ON DR.QE_Data_Row_Key=DI.QE_Data_Row_Key
		AND DR.QE_Session_Key=@QESessionKey
		AND DR.Processed=0
	WHERE DI.QE_Template_Field_Key=@TemplateFieldKey
	AND DI.Data_Display LIKE '%' + @SearchText + '%'

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESession_SearchAndReplace') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESession_SearchAndReplace'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESession_SearchAndReplace TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESession_SearchAndReplace TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESession_SearchAndReplace TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESession_SearchAndReplace TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESession_SearchAndReplace TO [Dev - JNCC SQL]
END

GO