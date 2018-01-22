/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDateInShortDateFormat]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetDateInShortDateFormat
GO

/*===========================================================================*\
  Description:	Returns a date string in the specified ShortDateFormat

  Parameters:	@Y					- Year part of date
				@M					- Month part of date
				@D					- Day part of date
				@ShortDateFormat	- Short Date Format

  Created:	18 September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/05/04 11:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetDateInShortDateFormat]
(
@Y INT,
@M INT,
@D INT,
@ShortDateFormat VARCHAR(20)
)
RETURNS VARCHAR(20)
WITH SCHEMABINDING

AS
BEGIN

DECLARE @ReturnValue VARCHAR(20)
	
SET @ReturnValue = @ShortDateFormat

--Translate long years
SET @ReturnValue = REPLACE(@ReturnValue, 'yyyy', CAST(@Y AS VARCHAR(4)))

--Translate short years
SET @ReturnValue = REPLACE(@ReturnValue, 'yy', RIGHT(CAST(@Y AS VARCHAR(4)),2))

--Translate long months - padded with leading zero
SET @ReturnValue = REPLACE(@ReturnValue, 'mm', 
	CASE LEN(CAST(@M AS VARCHAR(2)))
	WHEN 1 THEN 
		'0' + CAST(@M AS VARCHAR(2)) 
	ELSE 
		CAST(@M AS VARCHAR(2)) 
	END)

--Translate short months
SET @ReturnValue = REPLACE(@ReturnValue, 'm', CAST(@M AS VARCHAR(2)))

--Translate long days
SET @ReturnValue = REPLACE(@ReturnValue, 'dd', 
	CASE LEN(CAST(@D AS VARCHAR(2))) 
	WHEN 1 THEN 
		'0' + CAST(@D AS VARCHAR(2)) 
	ELSE 
		CAST(@D AS VARCHAR(2)) 
	END)

--Translate short days
SET @ReturnValue = REPLACE(@ReturnValue, 'd', CAST(@D AS VARCHAR(2)))

RETURN @ReturnValue

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetDateInShortDateFormat]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetDateInShortDateFormat'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetDateInShortDateFormat TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetDateInShortDateFormat TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetDateInShortDateFormat TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetDateInShortDateFormat TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetDateInShortDateFormat TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetDateInShortDateFormat TO [Dev - JNCC SQL]
	END
GO

