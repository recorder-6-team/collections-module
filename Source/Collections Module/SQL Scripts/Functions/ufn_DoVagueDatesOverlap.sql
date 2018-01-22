/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_DoVagueDatesOverlap]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_DoVagueDatesOverlap
GO

/*===========================================================================*\
  Description:	Returns true if the two VagueDate overlap, otherwise false

  Parameters:	@VagueDate_1_Start 		Start of first VagueDate
				@VagueDate_1_End		End of first VagueDate
				@VagueDate_2_Start		Start of second VagueDate
				@VagueDate_2_End		End of second VagueDate

  Created:	30 October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/04 11:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_DoVagueDatesOverlap]
(
@VagueDate_1_Start INT,
@VagueDate_1_End INT,
@VagueDate_2_Start INT,
@VagueDate_2_End INT
)

RETURNS BIT

AS
BEGIN

DECLARE @Result BIT

DECLARE @LowIntegerLimit INT
DECLARE @HighIntegerLimit INT

SET @LowIntegerLimit = -2147483648
SET @HighIntegerLimit = 2147483647

SET @VagueDate_1_Start = ISNULL(@VagueDate_1_Start, @LowIntegerLimit)
SET @VagueDate_1_End = ISNULL(@VagueDate_1_End, @HighIntegerLimit)

SET @VagueDate_2_Start = ISNULL(@VagueDate_2_Start, @LowIntegerLimit)
SET @VagueDate_2_End = ISNULL(@VagueDate_2_End, @HighIntegerLimit)

IF ((@VagueDate_1_Start BETWEEN @VagueDate_2_Start AND @VagueDate_2_End)
		OR (@VagueDate_1_End BETWEEN @VagueDate_2_Start AND @VagueDate_2_End)
		OR (@VagueDate_1_Start <= @VagueDate_2_Start AND @VagueDate_1_End >= @VagueDate_2_Start)
		OR (@VagueDate_1_Start <= @VagueDate_2_End AND @VagueDate_1_End >= @VagueDate_2_End))
	SET @Result = 1
ELSE
	SET @Result = 0

	RETURN @Result
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_DoVagueDatesOverlap]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_DoVagueDatesOverlap'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_DoVagueDatesOverlap TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_DoVagueDatesOverlap TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_DoVagueDatesOverlap TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_DoVagueDatesOverlap TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_DoVagueDatesOverlap TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_DoVagueDatesOverlap TO [Dev - JNCC SQL]
	END
GO

