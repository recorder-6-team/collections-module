/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESession_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESession_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new quick entry session, including a row for the 
							general tab.  Display caption auto generated from the template
							name and the date.

  Parameters:	@Key OUTPUT - inserted key
							@QE_Template_Key - key of template
							@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 8 $
    $Date: 18/01/11 9:44 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESession_Insert]
	@Key					INT		OUTPUT,
	@QE_Template_Key		CHAR(16),
	@Occurrence_Key			CHAR(16) = NULL,
	@Taxon_Occurrence_Key	CHAR(16) = NULL,
	@SessionID				CHAR(16)
AS

DECLARE @Item_Name VARCHAR(50)

-- Read template name, truncate if necessary
SELECT @Item_Name = 
	CASE WHEN Len(Item_Name)>38 THEN
		Left(Item_Name, 36) + '..'
	ELSE
		Item_Name
	END
FROM QE_Template
WHERE QE_Template_Key=@QE_Template_Key

-- and append the date
SET @Item_Name = 
		@Item_Name + ' ' + 
		CONVERT(VARCHAR(11), GETDATE(), 106)

-- Ensure name is unique
DECLARE @Duplicates INT
SELECT @Duplicates = COUNT(*) FROM QE_Session 
WHERE Item_Name=@Item_Name
OR Item_Name LIKE @Item_Name + ' (%)'

IF @Duplicates>0
BEGIN
	WHILE EXISTS(SELECT 1 FROM QE_Session WHERE Item_Name = @Item_Name + ' (' + CONVERT(VARCHAR(8), @Duplicates+1) + ')')
		SET @Duplicates = @Duplicates + 1
	SET @Item_Name = @Item_Name + ' (' + CONVERT(VARCHAR(8), @Duplicates+1) + ')'  
END	
  
INSERT INTO QE_Session (
		QE_Template_Key,
		Item_Name,
		Entered_Session_ID,
		Occurrence_Key,
		Taxon_Occurrence_Key
)
VALUES (
	@QE_Template_Key,
	@Item_Name,
	@SessionID,
	@Occurrence_Key,
	@Taxon_Occurrence_Key
)

SET @Key=SCOPE_IDENTITY()  

INSERT INTO QE_Data_Row (
	QE_Session_Key, 
	General, 
	Validated, 
	Processed, 
	Entered_Session_ID)
VALUES (
  @Key, 
  1, 
  0, 
  0, 
  @SessionID)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESession_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESession_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESession_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESession_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESession_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESession_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESession_Insert TO [Dev - JNCC SQL]
END

GO
