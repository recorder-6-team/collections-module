/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDateFromVagueDate]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetDateFromVagueDate
GO

/*===========================================================================*\
  Description:	Returns a Vague Date string in the specified ShortDateFormat

  Parameters:	@VagueDateStart		- Start of Vague Date
				@VagueDateEnd		- End of Vague Date
				@VagueDateType		- Type of Vague Date
				@ShortDateFormat	- Short Date Format

  Created:	18 September 2003

  Last revision information:
    $Revision: 5 $
    $Date: 6/05/04 11:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetDateFromVagueDate]
(
@VagueDateStart INT,
@VagueDateEnd INT,
@VagueDateType VARCHAR(2)
)
RETURNS varchar(50)

AS
BEGIN

DECLARE @ReturnValue VARCHAR(50)
DECLARE @ShortDateFormat VARCHAR(12)
SELECT @ShortDateFormat=DATA FROM SETTING WHERE [NAME]='ShortDates' 

IF @VagueDateType IS NULL GOTO FINISH

--Procedure starts here
DECLARE @Y1 INT
DECLARE @M1 INT
DECLARE @D1 INT

DECLARE @Y2 INT
DECLARE @M2 INT
DECLARE @D2 INT

IF (@VagueDateStart IS NOT NULL)
BEGIN
	SET @Y1 = dbo.ufn_GetDatePartFromInteger(@VagueDateStart, 'y')
	SET @M1 = dbo.ufn_GetDatePartFromInteger(@VagueDateStart, 'm')
	SET @D1 = dbo.ufn_GetDatePartFromInteger(@VagueDateStart, 'd')
END

IF (@VagueDateEnd IS NOT NULL)
BEGIN
	SET @Y2 = dbo.ufn_GetDatePartFromInteger(@VagueDateEnd, 'y')
	SET @M2 = dbo.ufn_GetDatePartFromInteger(@VagueDateEnd, 'm')
	SET @D2 = dbo.ufn_GetDatePartFromInteger(@VagueDateEnd, 'd')
END


DECLARE @StartDate VARCHAR(20)
DECLARE @EndDate VARCHAR(20)

IF @VagueDateType = 'D'
BEGIN
  --Handle day
	IF @VagueDateStart IS NOT NULL
		SET @ReturnValue = dbo.ufn_GetDateInShortDateFormat(@Y1, @M1, @D1, @ShortDateFormat)
END
ELSE IF @VagueDateType = 'DD'
BEGIN
  --Handle day range vague dates
	IF @VagueDateStart IS NOT NULL AND @VagueDateEnd IS NOT NULL
	BEGIN
		SET @StartDate = dbo.ufn_GetDateInShortDateFormat(@Y1, @M1, @D1, @ShortDateFormat)
		SET @EndDate = dbo.ufn_GetDateInShortDateFormat(@Y2, @M2, @D2, @ShortDateFormat)
		SET @ReturnValue = @StartDate + ' - ' + @EndDate
	END
END
ELSE BEGIN
	--Handle all other vague dates
	SET @ReturnValue =
		CASE @VagueDateType 
		  WHEN 'U' THEN 'Unknown'
	    --Day - to convert to a day, we use a dummy year 2003, then correct it to the original year.  
			--This avoids problems with dates prior to 1753
		  WHEN 'Y' THEN CAST(@Y1 AS VARCHAR(4))
		  WHEN 'YY' THEN CAST(@Y1 AS VARCHAR(4)) + ' - ' + CAST(@Y2 AS VARCHAR(4))
		  WHEN 'Y-' THEN CAST(@Y1 AS VARCHAR(4)) + ' -'
		  WHEN '-Y' THEN '- ' + CAST(@Y2 AS VARCHAR(4))
			WHEN 'C' THEN CAST(ROUND(@Y1/100,0)+1 AS VARCHAR(2))+'c'
			WHEN 'CC' THEN CAST(ROUND(@Y1/100,0)+1 AS VARCHAR(2))+'c - ' + CAST(ROUND(@Y2/100,0)+1 AS VARCHAR(2)) + 'c'
			WHEN 'C-' THEN CAST(ROUND(@Y1/100,0)+1 AS VARCHAR(2))+'c -'
			WHEN '-C' THEN '- ' + CAST(ROUND(@Y2/100,0)+1 AS VARCHAR(2)) + 'c'
			WHEN 'O' THEN dbo.ufn_MonthToPreferredMonth(@M1) + ' ' + CAST(@Y1 AS VARCHAR(4))
			WHEN 'OO' THEN 
					dbo.ufn_MonthToPreferredMonth(@M1) + ' ' + CAST(@Y1 AS VARCHAR(4)) + ' - ' +
					dbo.ufn_MonthToPreferredMonth(@M2) + ' ' + CAST(@Y2 AS VARCHAR(4))
			WHEN 'M' THEN dbo.ufn_MonthToPreferredMonth(@M1)
			WHEN 'S' THEN dbo.ufn_MonthToSeason(@M1)
			WHEN 'P' THEN dbo.ufn_MonthToSeason(@M1) + ' ' + CAST(@Y1 AS VARCHAR(4))
		END
END
FINISH:
RETURN @ReturnValue

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDateFromVagueDate]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetDateFromVagueDate'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetDateFromVagueDate TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetDateFromVagueDate TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetDateFromVagueDate TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetDateFromVagueDate TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetDateFromVagueDate TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetDateFromVagueDate TO [Dev - JNCC SQL]
	END
GO
