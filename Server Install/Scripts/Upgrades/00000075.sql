
/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.ufn_GeneratePublishedTermBasicTermRankAuthority') IS NOT NULL
	DROP FUNCTION dbo.ufn_GeneratePublishedTermBasicTermRankAuthority
GO

/*============================================================================*\
	Description:
		Function used to calculate a published term in the format
		<i>[Basic Term]</i> [Rank Abbreviation] <i>[Basic Term]</i> [Authority]
		See CCN 165

	Created: February 2014

	Last revision information:
		$Revision: 1 $
		$Date: 18/02/15 19:39 $
		$Author: Simonwood $
\*============================================================================*/

CREATE FUNCTION dbo.ufn_GeneratePublishedTermBasicTermRankAuthority
(
	@Plaintext NVARCHAR(150),
	@AuthorAndDate VARCHAR(100),
	@Attributes VARCHAR(100),
	@RankKey CHAR(16),
	@ParentConceptKey CHAR(16)
)
RETURNS NVARCHAR(256)
AS
BEGIN
	DECLARE @RankAbbreviation  VARCHAR(10)
	DECLARE @ParentAuthority VARCHAR(100)
	DECLARE @LastSpace INT
	DECLARE @Len INT

	SET @PlainText = LTRIM(RTRIM(@Plaintext))

	SET @RankAbbreviation = (SELECT Abbreviation
		FROM dbo.Concept_Rank
		WHERE Concept_Rank_Key = @RankKey)
	SET @ParentAuthority = (SELECT cg.Authority
		FROM dbo.Concept_Group cg
		INNER JOIN dbo.Concept c
		ON cg.Concept_Group_Key = c.Concept_Group_Key
		WHERE c.Concept_Key = @ParentConceptKey)

	
	SET @LastSpace = CharIndex(' ', REVERSE(@Plaintext))
	SET @Len = LEN(@Plaintext)


	RETURN '<i>' + LEFT(@Plaintext, @Len -@LastSpace)  + '</i> ' + ISNULL(@RankAbbreviation, '') + '<i>'  + 
	RIGHT(@Plaintext, @LastSpace) +  '</i> '
	+ ISNULL(@AuthorAndDate, '')
END
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on function ufn_GeneratePublishedTermBasicTermRankAuthority'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO "Dev - JNCC SQL"
GO

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESessionLastUser_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESessionLastUser_Select]
GO
    

/*===========================================================================*\
  Description:	Returns the last user and datetime edited of a quick entry session

  Parameters:	
		@QESessionKey - @QE_SEssion_Key

  Created:	March 2014

  Last revision information:
    $Revision: 1 $
    $Date: 18/02/15 19:39 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionLastUser_Select]
  @QESessionKey as int
AS

SET NOCOUNT ON
IF EXISTS (
	SELECT QS.Changed_Session_ID
	FROM QE_Session QS
	WHERE QS.QE_Session_Key = @QESessionKey
    AND QS.Changed_Session_ID IS NOT NULL)

    SELECT dbo.ufn_GetFormattedName(S.User_Name_Key) + 
			' - (' + CAST (ISNULL(S.[Date_Time_End],[Date_Time_Start]) as varchar(20))  + ' )'  AS [User_Name]
	FROM QE_Session QS
	INNER JOIN Session S 
		ON QS.Changed_Session_ID = S.Session_ID
	WHERE QS.QE_Session_Key = @QESessionKey
ELSE
	SELECT dbo.ufn_GetFormattedName(S.User_Name_Key) + 
	' - (' + CAST (ISNULL(S.[Date_Time_End],[Date_Time_Start]) as varchar(20))  + ' )' AS [User_Name]
	FROM QE_Session QS
	INNER JOIN Session S 
		ON QS.Entered_Session_ID = S.Session_ID
	WHERE QS.QE_Session_Key = @QESessionKey  
		
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionLastUser_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionLastUser_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_QESessionLastUser_Select TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QEDataItem_Insert]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QEDataItem_Insert]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 18/02/15 19:39 $
    $Author: Simonwood $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QEDataItem_Insert]
  @QEDataRowKey as int,
  @QETemplateFieldKey as char(16),
  @DataValue as varchar(200),
  @DataDisplay as Varchar(200),
  @SessionID as char(16)
 AS

SET NOCOUNT ON

IF NOT EXISTS(SELECT 1 FROM QE_Data_Item 
		WHERE QE_Data_Row_Key=@QEDataRowKey
		AND QE_Template_Field_Key=@QETemplateFieldKey)
BEGIN
	INSERT INTO QE_Data_Item (QE_Data_Row_Key, QE_Template_Field_Key,
			Data_Value, Data_Display, Entered_Session_ID)
		VALUES(@QEDataRowKey, @QETemplateFieldKey, @DataValue, @DataDisplay, @SessionID)

	SELECT QE_Data_Item_Key, Timestamp 
	FROM QE_Data_Item 
	WHERE	QE_Data_Item_Key = Scope_Identity()
END
ELSE 
BEGIN
  -- Item already exists so just update it
	DECLARE @QEDataItemKey CHAR(16)

  SELECT @QEDataItemKey=QE_Data_Item_Key
	FROM QE_Data_Item
	WHERE QE_Data_Row_Key=@QEDataRowKey
		AND QE_Template_Field_Key=@QETemplateFieldKey
	
	UPDATE QE_Data_Item
		SET Data_Value = @DataValue,
		Data_Display = @DataDisplay,
		Changed_Session_ID = @SessionID
	WHERE	 QE_Data_Item_Key = @QEDataItemKey
	
	SELECT QE_Data_Item_Key, Timestamp 
	FROM QE_Data_Item 
	WHERE	QE_Data_Item_Key = @QEDataItemKey
END

Update QE_Session
		set Changed_Session_ID = @SessionID
From QE_Data_Row QR
INNER JOIN QE_Session QS ON QS.QE_Session_Key = QR.QE_Session_Key
	where 
		QE_Data_Row_Key = @QEDataRowKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QEDataItem_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QEDataItem_Update]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 18/02/15 19:39 $
    $Author: Simonwood $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QEDataItem_Update]
  @QEDataItemKey as int,
  @DataValue as varchar(200),
  @DataDisplay as varchar(200),
  @Timestamp as timestamp,
  @SessionID as char(16)

 AS

	SET NOCOUNT OFF

	Update QE_Data_Item
		set Data_Value = @DataValue,
		Data_Display = @DataDisplay,
		Changed_Session_ID = @SessionID
	where 
		([Timestamp] = @Timestamp OR @Timestamp IS NULL)
		and (QE_Data_Item_Key = @QEDataItemKey)

	IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Data_Item WHERE QE_Data_Item_Key = @QEDataItemKey)
		RAISERROR('Record updated by another user', 16, 1)

	Update QE_Session
		set Changed_Session_ID = @SessionID
	From QE_Data_Item QD
	INNER JOIN QE_Data_Row QR ON QD.QE_Data_Row_Key = QR.QE_Data_Row_Key
	INNER JOIN QE_Session QS ON QS.QE_Session_Key = QR.QE_Session_Key
	where 
		QE_Data_Item_Key = @QEDataItemKey

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [Dev - JNCC SQL]
END

GO