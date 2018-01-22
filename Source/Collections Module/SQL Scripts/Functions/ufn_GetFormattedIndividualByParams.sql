/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedIndividualByParams]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedIndividualByParams
GO

/*===========================================================================*\
  Description:	Returns the individual's formatted name for the given parameters.
		The rule for individual name format is:
			Title + (Forename | Initials) + Surname

		Null fields are omitted.

  Parameters:	@Title:		Title
				@Initials:	Initials
				@Forename:	Forname
				@Surname:	Surname

  Created:	27 August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/04 11:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedIndividualByParams]
(
@Title VARCHAR(4),
@Initials VARCHAR(8),
@ForeName VARCHAR(20),
@Surname VARCHAR(30)
)
RETURNS varchar(100)


AS
BEGIN
	DECLARE	@FormattedName varchar(100)
	SET @FormattedName = ''

	SET @FormattedName = 
		CASE WHEN @Forename IS NULL THEN
			CASE WHEN @Initials IS NULL THEN
				CASE WHEN @Title IS NULL THEN 
					@Surname
				ELSE 
					@Title + ' ' + @Surname
				END
			ELSE
				@Initials + ' ' + @Surname
			END
		ELSE 
			@Forename + ' ' + @Surname 
		END

	RETURN @FormattedName
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedIndividualByParams]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetFormattedIndividualByParams'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedIndividualByParams TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetFormattedIndividualByParams TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetFormattedIndividualByParams TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedIndividualByParams TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedIndividualByParams TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedIndividualByParams TO [Dev - JNCC SQL]
	END
GO
