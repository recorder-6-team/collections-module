/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_MonthToSeason]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_MonthToSeason
GO

/*===========================================================================*\
  Description:	Converts a Month into a Season

  Parameters:	@Month		- Month number

  Created:	18 September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 23/02/07 10:37 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_MonthToSeason(
  @Month INT
)
RETURNS varchar(150)

AS
BEGIN

	DECLARE @Output VARCHAR(6)
	DECLARE @Meaning_Key CHAR(16)

	IF @Month=1 OR @Month=2 OR @Month=12 
	  SET @Meaning_Key = 'SYSTEM000000008E'
	ELSE IF @Month=3 OR @Month=4 OR @Month=5
	  SET @Meaning_Key = 'SYSTEM000000008F'
	ELSE IF @Month=6 OR @Month=7 OR @Month=8
	  SET @Meaning_Key = 'SYSTEM000000008G'
	ELSE IF @Month=9 OR @Month=10 OR @Month=11
	  SET @Meaning_Key = 'SYSTEM000000008H'
	
	SELECT TOP 1 @Output = T.PlainText 
	FROM Concept C
	INNER JOIN Term T ON T.Term_Key=C.Term_Key
	INNER JOIN Language L ON l.Language_Key=T.Language_Key
	WHERE Meaning_Key = @Meaning_Key
	ORDER BY L.Priority ASC

	RETURN @Output

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_MonthToSeason]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_MonthToSeason'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [Dev - JNCC SQL]
	END
GO
