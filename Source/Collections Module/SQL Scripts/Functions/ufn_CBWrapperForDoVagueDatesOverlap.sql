/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_CBWrapperForDoVagueDatesOverlap]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_CBWrapperForDoVagueDatesOverlap
GO

/*===========================================================================*\
  Description:	Provides a wrapper to the function dbo.DoVagueDatesOverlap
				to replace zeros contained in @VagueDate_CB_Start, @VagueDate_CB_End
				with NULLs if the VagueDate_CB_Type is Y-, -Y, C- or -C

  Parameters:	@VagueDate_CB_Start 	Start of VagueDate from CollectionsBrowser
				@VagueDate_CB_End		End of VagueDate from CollectionsBrowser
				@VagueDate_CB_Type		Type of VagueDate from CollectionsBrowser
				@VagueDate_DB_Start		Start of VagueDate from Database
				@VagueDate_DB_End		End of VagueDate from Database

  Created:	30 October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/05/04 11:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_CBWrapperForDoVagueDatesOverlap]
(
@VagueDateString VARCHAR(30),
@VagueDate_DB_Start INT,
@VagueDate_DB_End INT
)

RETURNS BIT

AS
BEGIN


DECLARE @VagueDate_CB_Start INT
DECLARE @VagueDate_CB_End INT
DECLARE @VagueDate_CB_Type VARCHAR(2)
DECLARE @liDateSeperatorIndex INT
DECLARE @liDateTypeSeperatorIndex INT

SET @liDateSeperatorIndex = CHARINDEX(':', @VagueDateString)
SET @liDateTypeSeperatorIndex = CHARINDEX(';', @VagueDateString)
SET @VagueDate_CB_Start = CAST(SUBSTRING(@VagueDateString, 1, @liDateSeperatorIndex -1) AS INT)
SET @VagueDate_CB_End = CAST(SUBSTRING(@VagueDateString, @liDateSeperatorIndex +1, @liDateTypeSeperatorIndex - @liDateSeperatorIndex -1) AS INT)
SET @VagueDate_CB_Type = SUBSTRING(@VagueDateString, @liDateTypeSeperatorIndex +1, LEN(@VagueDateString) -@liDateTypeSeperatorIndex)

IF (UPPER(@VagueDate_CB_Type) = 'Y-') OR (UPPER(@VagueDate_CB_Type) = 'C-')
	SET @VagueDate_CB_End = NULL
ELSE IF (UPPER(@VagueDate_CB_Type) = '-Y') OR (UPPER(@VagueDate_CB_Type) = '-C')
	SET @VagueDate_CB_Start = NULL

RETURN dbo.ufn_DoVagueDatesOverlap(@VagueDate_CB_Start, @VagueDate_CB_End, @VagueDate_DB_Start, @VagueDate_DB_End)
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_CBWrapperForDoVagueDatesOverlap]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_CBWrapperForDoVagueDatesOverlap'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_CBWrapperForDoVagueDatesOverlap TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_CBWrapperForDoVagueDatesOverlap TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_CBWrapperForDoVagueDatesOverlap TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_CBWrapperForDoVagueDatesOverlap TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_CBWrapperForDoVagueDatesOverlap TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_CBWrapperForDoVagueDatesOverlap TO [Dev - JNCC SQL]
	END
GO
